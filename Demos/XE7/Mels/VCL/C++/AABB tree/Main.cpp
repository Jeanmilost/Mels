/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : 3D ray picking with AABB tree simplification demo main form                      *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// std
#include <sstream>
#include <memory>

// Mels library
#include <UTQRGeometry.hpp>
#include <UTQR3D.hpp>

// engine
#include "QR_MathsHelper.h"
#include "QR_OpenGLHelper.h"

#pragma package(smart_init)
#pragma link "UTQRCollision"
#pragma link "UTQRShapeGroup"
#pragma link "UTQRGeometry"
#pragma link "UTQR3D"
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
{}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    // delete AABB tree
    if (m_pAABBTree)
        delete m_pAABBTree;

    // delete sphere model
    if (m_pSphere)
        delete m_pSphere;

    // shutdown OpenGL
    QR_OpenGLHelper::DisableOpenGL(Handle, m_hDC, m_hRC);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject* pSender)
{
    // was OpenGL already initialized?
    if (m_hRC)
        return;

    // initialize OpenGL
    if (!QR_OpenGLHelper::EnableOpenGL(Handle, m_hDC, m_hRC))
    {
        MessageDlg("OpenGL could not be initialized.\r\n\r\nApplication will close.", mtError,
                TMsgDlgButtons() << mbOK, 0);;
        Application->Terminate();
        return;
    }

    // configure OpenGL
    ConfigOpenGL();
    QR_OpenGLHelper::CreateViewport(ClientWidth, ClientHeight, false);

    // create and configure sphere
    m_pSphere                   = new TQRSphereGroup();
    m_pSphere->OnCustomDrawItem = OnDrawCustomStaticModelItem;

    std::auto_ptr<TQRColor> pColor(new TQRColor(0, 0, 255, 255));

    m_pSphere->Load(20,
                    20,
                    1.0f,
                    pColor.get(),
                    TQRModelOptions() << EQR_MO_Without_Normals
                                      << EQR_MO_Without_Textures);

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
        // calculate full max angle (i.e. 360°)
        const float fullAngle = M_PI * 2.0f;

        // calculate next rotation angle
        m_Theta = (m_Theta + 0.008f > fullAngle) ? ((m_Theta + 0.008f) - fullAngle) : m_Theta + 0.008f;
    }

    m_pSphere->RotationY = m_Theta;

    m_pSphere->Draw(elapsedTime);
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

    // enable culling
    glDisable(GL_CULL_FACE);
    glCullFace(GL_NONE);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnDrawCustomStaticModelItem(TQRModelGroup* const pGroup,
                                                       TQRModel*            pModel,
                                                       const TQRTextures    textures,
                                                       const TQRMatrix4x4&  matrix)
{
    // no model?
    if (!pModel)
        return;

    // model mesh still not created?
    if (!m_Mesh.Length && !m_pAABBTree)
    {
        // get sphere model
        TQRSphereModel* pSphereModel = static_cast<TQRSphereModel*>(pModel);

        // found it?
        if (!pSphereModel)
            return;

        // create new aligned-axis bouding box tree
        m_pAABBTree = new TQRAABBTree();

        // get sphere mesh to draw and aligned-axis bounding box tree to use
        pSphereModel->GetMesh(m_Mesh, m_pAABBTree);
    }

    // draw mesh
    QR_OpenGLHelper::Draw(m_Mesh, matrix, textures);

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
            // ad polygon in collision to resulting list
            polygonToDraw.Length                   += 1;
            polygonToDraw[polygonToDraw.Length - 1] = polygons[i];
        }

    const std::size_t polygonToDrawCount = polygonToDraw.Length;

    glPushMatrix();

    // unfortunately, because Delphi don't allow to declare a const function as in C++
    TQRMatrix4x4* pModelMatrix = const_cast<TQRMatrix4x4*>(&matrix);

    // place triangles into 3D world
    glLoadMatrixf(pModelMatrix->GetPtr());

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
