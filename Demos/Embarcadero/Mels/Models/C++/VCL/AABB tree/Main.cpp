// *************************************************************************************************
// * ==> Main -------------------------------------------------------------------------------------*
// *************************************************************************************************
// * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
// *                                                                                               *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
// * and associated documentation files (the "Software"), to deal in the Software without          *
// * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
// * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
// * Software is furnished to do so, subject to the following conditions:                          *
// *                                                                                               *
// * The above copyright notice and this permission notice shall be included in all copies or      *
// * substantial portions of the Software.                                                         *
// *                                                                                               *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
// * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
// * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
// *************************************************************************************************

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// std
#include <sstream>
#include <memory>
#include <algorithm>

// Mels library
#include <UTQRGraphics.hpp>
#include <UTQRGeometry.hpp>
#include <UTQR3D.hpp>

// engine
#include "QR_MathsHelper.h"
#include "QR_OpenGLHelper.h"

#pragma package(smart_init)
#pragma link "UTQRCollision"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_hDC(NULL),
    m_hRC(NULL),
    m_pAABBTree(NULL),
    m_PreviousTime(0),
    m_CollidePolygonsCount(0),
    m_HighestHit(0),
    m_Theta(0.0f),
    m_Rotate(false)
{
    std::auto_ptr<TQRColor> pColor(new TQRColor(0, 0, 255, 255));

    // create a demo blue sphere
    std::auto_ptr<TQRSphereModel> pSphereModel(new TQRSphereModel());
    pSphereModel->Slices       = 20;
    pSphereModel->Stacks       = 20;
    pSphereModel->Radius       = 1.0f;
    pSphereModel->Color        = pColor.get();
    pSphereModel->VertexFormat = TQRVertexFormat() << EQR_VF_Colors;

    // create aligned-axis bounding box tree
    m_pAABBTree = new TQRAABBTree();

    pSphereModel->GetMesh(m_Mesh, m_pAABBTree);
}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    // delete aabb tree
    if (m_pAABBTree)
        delete m_pAABBTree;

    // shutdown OpenGL
    QR_OpenGLHelper::DisableOpenGL(Handle, m_hDC, m_hRC);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject* pSender)
{
    // was OpenGL already initialized?
    if (m_hRC)
        return;

    // initialize OpenGL
    if (!QR_OpenGLHelper::EnableOpenGL(Handle, m_hDC, m_hRC))
    {
        MessageDlg("OpenGL could not be initialized.\r\n\r\nApplication will close.",
                   mtError,
                   TMsgDlgButtons() << mbOK,
                   0);

        Application->Terminate();
        return;
    }

    // configure OpenGL
    ConfigOpenGL();
    QR_OpenGLHelper::CreateViewport(ClientWidth, ClientHeight, false);

    // from now, OpenGL will draw scene every time the thread do nothing else
    Application->OnIdle = OnIdle;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormPaint(TObject *Sender)
{
    RenderGLScene();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::acRotateExecute(TObject* pSender)
{
    acRotate->Checked = !acRotate->Checked;
    m_Rotate          =  acRotate->Checked;
    btRotate->Caption =  m_Rotate ? L"Stop" : L"Rotate";
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::RenderGLScene()
{
    // calculate time interval
    const std::time_t now            = ::GetTickCount();
    const double      elapsedTime    = (now - m_PreviousTime);
                      m_PreviousTime =  now;

    // clear scene
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // draw scene
    DrawScene(elapsedTime);

    glFlush();

    // finalize scene
    ::SwapBuffers(m_hDC);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::DrawScene(const double& elapsedTime)
{
    // do rotate sphere?
    if (m_Rotate)
    {
        // calculate next rotation angle
        m_Theta = m_Theta + (0.001f * elapsedTime);

        // correct it if out of bounds
        if (m_Theta > (M_PI * 2.0f))
            m_Theta -= (M_PI * 2.0f);
    }

    TQRTextures textures;

    // draw mesh
    QR_OpenGLHelper::Draw(m_Mesh,
                          TQRVector3D(0.0f, 0.0f, 0.0f),
                          0.0f,
                          m_Theta,
                          0.0f,
                          TQRVector3D(1.0f, 1.0f, 1.0f),
                          textures);

    TQRRect rect(-1.0f, 1.0f, 2.0f, 2.0f);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    TQRVector3D rayPos = QR_OpenGLHelper::MousePosToGLPoint(Handle, rect);
    TQRVector3D rayDir(0.0f, 0.0f, 1.0f);

    // prepare rotation matrix
    TQRMatrix4x4 rotateMatrix = TQRMatrix4x4::Identity();
    rotateMatrix.Rotate(-m_Theta, TQRVector3D(0.0f, 1.0f, 0.0f));

    // rotate ray position and direction
    rayPos = rotateMatrix.Transform(rayPos);
    rayDir = rotateMatrix.Transform(rayDir);

    // create and populate ray from mouse position
    std::auto_ptr<TQRRay> pRay(new TQRRay());
    pRay->Pos = &rayPos;
    pRay->Dir = &rayDir;

    TQRPolygons polygons;

    // get polygons to check for collision by resolving AABB tree
    m_pAABBTree->Resolve(pRay.get(), polygons);

    const std::size_t polygonCount = polygons.Length;
          TQRPolygons polygonToDraw;

    // update highest hit
    m_HighestHit = std::max(m_HighestHit, polygonCount);

    // iterate through polygons to check
    for (std::size_t i = 0; i < polygonCount; ++i)
        // is polygon intersecting ray?
        if (TQRCollisionHelper::GetRayPolygonCollision(pRay.get(), polygons[i]))
        {
            // add polygon in collision to resulting list
            polygonToDraw.Length                   += 1;
            polygonToDraw[polygonToDraw.Length - 1] = polygons[i];
        }

    const std::size_t polygonToDrawCount = polygonToDraw.Length;

    glPushMatrix();

    // place triangles into 3D world
    glTranslatef(0.0f, 0.0f, 0.0f);
    glRotatef(0.0f, 1.0, 0.0, 0.0);
    glRotatef(QR_MathsHelper::RadToDeg(m_Theta), 0.0, 1.0, 0.0);
    glRotatef(0.0f, 0.0, 0.0, 1.0);
    glScalef(1.0f, 1.0f, 1.0f);

    // found collide polygons to draw?
    for (unsigned i = 0; i < polygonToDrawCount; ++i)
    {
        glBegin(GL_TRIANGLES);

        // draw vertex 1
        glColor3f(1.0f, 0.0f, 0.0f);
        glVertex3f(polygonToDraw[i].Vertex1->X,
                   polygonToDraw[i].Vertex1->Y,
                   polygonToDraw[i].Vertex1->Z);

        // draw vertex 2
        glColor3f(0.8f, 0.0f, 0.2f);
        glVertex3f(polygonToDraw[i].Vertex2->X,
                   polygonToDraw[i].Vertex2->Y,
                   polygonToDraw[i].Vertex2->Z);

        // draw vertex 3
        glColor3f(1.0f, 0.12f, 0.2f);
        glVertex3f(polygonToDraw[i].Vertex3->X,
                   polygonToDraw[i].Vertex3->Y,
                   polygonToDraw[i].Vertex3->Z);

        glEnd();

        glFlush();
    }

    glPopMatrix();

    // show collision detection status
    ShowStatus(polygonCount, polygonToDrawCount);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnIdle(TObject* pSender, bool& done)
{
    done = false;

    // draw OpenGL scene every time application do nothing
    RenderGLScene();
}
//--------------------------------------------------------------------------------------------------
void TMainForm::ShowStatus(int toTest, int inCollision) const
{
    // show collision detection status
    laTotal->Caption       = L"Total: "        + ::IntToStr((int)m_CollidePolygonsCount);
    laToTest->Caption      = L"To test: "      + ::IntToStr(toTest);
    laInCollision->Caption = L"In collision: " + ::IntToStr(inCollision);
    laHighestHit->Caption  = L"Highest hit: "  + ::IntToStr((int)m_HighestHit);
}
//--------------------------------------------------------------------------------------------------
void TMainForm::ConfigOpenGL()
{
    // configure OpenGL depth testing
    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_TRUE);
    glDepthFunc(GL_LEQUAL);
    glDepthRange(0.0f, 1.0f);

    // disable culling
    glDisable(GL_CULL_FACE);
    glCullFace(GL_NONE);
}
//--------------------------------------------------------------------------------------------------
