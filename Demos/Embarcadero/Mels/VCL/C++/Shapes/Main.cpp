/*************************************************************************************************
 * ==> Main -------------------------------------------------------------------------------------*
 *************************************************************************************************
 * Description : 3D geometrical shapes demo main form                                            *
 * Developer   : Jean-Milost Reymond                                                             *
 *************************************************************************************************
 * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
 *                                                                                               *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
 * and associated documentation files (the "Software"), to deal in the Software without          *
 * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
 * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
 * Software is furnished to do so, subject to the following conditions:                          *
 *                                                                                               *
 * The above copyright notice and this permission notice shall be included in all copies or      *
 * substantial portions of the Software.                                                         *
 *                                                                                               *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
 *************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// vcl
#include <Vcl.Graphics.hpp>

// std
#include <memory>
#include <string>

// engine
#include "QR_OpenGLHelper.h"

// resources
#include "Resources.rh"

#pragma package(smart_init)
#pragma link "UTQRShapeGroup"
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
    m_pSurface(NULL),
    m_pSphere(NULL),
    m_pBox(NULL),
    m_pCone(NULL),
    m_pTorus(NULL),
    m_pParabola(NULL),
    m_pCylinder(NULL),
    m_pPyramid(NULL),
    m_pTetrahedron(NULL),
    m_PreviousTime(::GetTickCount())
{
    miLighting->Checked = true;
}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    // delete surface
    if (m_pSurface)
        delete m_pSurface;

    // delete sphere
    if (m_pSphere)
        delete m_pSphere;

    // delete box
    if (m_pBox)
        delete m_pBox;

    // delete cone
    if (m_pCone)
        delete m_pCone;

    // delete torus
    if (m_pTorus)
        delete m_pTorus;

    // delete parabola
    if (m_pParabola)
        delete m_pParabola;

    // delete cylinder
    if (m_pCylinder)
        delete m_pCylinder;

    // delete pyramid
    if (m_pPyramid)
        delete m_pPyramid;

    // delete tetrahedron
    if (m_pTetrahedron)
        delete m_pTetrahedron;

    // shutdown OpenGL
    QR_OpenGLHelper::DisableOpenGL(Handle, m_hDC, m_hRC);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject* pSender)
{
    // initialize OpenGL
    if (!QR_OpenGLHelper::EnableOpenGL(Handle, m_hDC, m_hRC))
    {
        MessageDlg("OpenGL could not be initialized.\r\n\r\nApplication will close.", mtError,
                TMsgDlgButtons() << mbOK, 0);
        Application->Terminate();
        return;
    }

    // configure OpenGL
    ConfigOpenGL();

    // create and configure surface
    m_pSurface                    = new TQRSurfaceGroup();
    m_pSurface->OnLoadMeshTexture = OnLoadMeshTexture;
    m_pSurface->OnCustomDrawItem  = OnDrawCustomStaticModelItem;

    std::auto_ptr<TQRColor> pSurfaceColor(new TQRColor(255, 255, 255, 255));

    // create and configure sphere
    m_pSurface->OnCustomDrawItem = OnDrawCustomStaticModelItem;

    // load sphere model
    m_pSurface->Load(0.15f,
                     0.15f,
                     pSurfaceColor.get(),
                     TQRModelOptions() << EQR_MO_Without_Colors);

    TQRVector3D surfaceTranslation(-0.2f, 0.2f, -1.0f);

    // locate model in world
    m_pSurface->Translation = &surfaceTranslation;

    // create and configure sphere
    m_pSphere                    = new TQRSphereGroup();
    m_pSphere->OnLoadMeshTexture = OnLoadMeshTexture;
    m_pSphere->OnCustomDrawItem  = OnDrawCustomStaticModelItem;

    std::auto_ptr<TQRColor> pSphereColor(new TQRColor(255, 255, 255, 255));

    // create and configure sphere
    m_pSphere->OnCustomDrawItem = OnDrawCustomStaticModelItem;

    // load sphere model
    m_pSphere->Load(20,
                    20,
                    0.09f,
                    pSphereColor.get(),
                    TQRModelOptions() << EQR_MO_Without_Colors);

    TQRVector3D sphereTranslation(0.0f, 0.2f, -1.0f);

    // locate model in world
    m_pSphere->Translation = &sphereTranslation;
    m_pSphere->RotationX   = -M_PI / 2.0f;

    // create and configure box
    m_pBox                    = new TQRBoxGroup();
    m_pBox->OnCustomDrawItem  = OnDrawCustomStaticModelItem;
    m_pBox->OnLoadMeshTexture = OnLoadMeshTexture;

    std::auto_ptr<TQRColor> pBoxColor(new TQRColor(255, 255, 255, 255));

    // load box model
    m_pBox->Load(0.11f,
                 0.11f,
                 0.11f,
                 pBoxColor.get(),
                 false,
                 TQRModelOptions() << EQR_MO_Without_Colors);

    TQRVector3D boxTranslation(0.2f, 0.2f, -1.0f);

    // locate model in world
    m_pBox->Translation = &boxTranslation;
    m_pBox->RotationX   =  M_PI / 5.0f;
    m_pBox->RotationY   =  M_PI + M_PI / 3.0f;

    // create and configure cone
    m_pCone                    = new TQRConeGroup();
    m_pCone->OnCustomDrawItem  = OnDrawCustomStaticModelItem;
    m_pCone->OnLoadMeshTexture = OnLoadMeshTexture;

    std::auto_ptr<TQRColor> pConeColor(new TQRColor(255, 255, 255, 255));

    // load cone model
    m_pCone->Load(20,
                  0.15f,
                  0.055f,
                  0.055f,
                  0.1f,
                  0.1f,
                  EQR_CC_Both,
                  pConeColor.get(),
                  TQRModelOptions() << EQR_MO_Without_Colors);

    TQRVector3D coneTranslation(-0.2f, 0.0f, -1.0f);

    // locate model in world
    m_pCone->Translation = &coneTranslation;

    // create and configure torus
    m_pTorus                    = new TQRTorusGroup();
    m_pTorus->OnCustomDrawItem  = OnDrawCustomStaticModelItem;
    m_pTorus->OnLoadMeshTexture = OnLoadMeshTexture;

    std::auto_ptr<TQRColor> pTorusColor(new TQRColor(255, 255, 255, 255));

    // load torus model
    m_pTorus->Load(20,
                   20,
                   0.075f,
                   0.075f,
                   0.025f,
                   0.025f,
                   pTorusColor.get(),
                   TQRModelOptions() << EQR_MO_Without_Colors);

    TQRVector3D torusTranslation(0.0f, 0.0f, -1.0f);

    // locate model in world
    m_pTorus->Translation = &torusTranslation;
    m_pTorus->RotationX   =  M_PI / 1.5f;
    m_pTorus->RotationY   =  M_PI / 5.0f;

    // create and configure parabola
    m_pParabola                    = new TQRParabolaGroup();
    m_pParabola->OnCustomDrawItem  = OnDrawCustomStaticModelItem;
    m_pParabola->OnLoadMeshTexture = OnLoadMeshTexture;

    std::auto_ptr<TQRColor> pParabolaColor(new TQRColor(255, 255, 255, 255));

    // load parabola model
    m_pParabola->Load(20,
                      20,
                      0.04f,
                      0.011f,
                      pParabolaColor.get(),
                      TQRModelOptions() << EQR_MO_Without_Colors);

    TQRVector3D parabolaTranslation(0.22f, -0.03f, -1.0f);

    // locate model in world
    m_pParabola->Translation = &parabolaTranslation;
    m_pParabola->RotationX   =  M_PI + (M_PI / 1.5f);
    m_pParabola->RotationY   = -M_PI / 5.0f;

    // create and configure cylinder
    m_pCylinder                    = new TQRConeGroup();
    m_pCylinder->OnCustomDrawItem  = OnDrawCustomStaticModelItem;
    m_pCylinder->OnLoadMeshTexture = OnLoadMeshTexture;

    std::auto_ptr<TQRColor> pCylinderColor(new TQRColor(255, 255, 255, 255));

    // load cylinder model (it's a cone where top and bottom radius are equals)
    m_pCylinder->Load(20,
                      0.15f,
                      0.06f,
                      0.06f,
                      0.06f,
                      0.06f,
                      EQR_CC_Both,
                      pCylinderColor.get(),
                      TQRModelOptions() << EQR_MO_Without_Colors);

    TQRVector3D cylinderTranslation(-0.2f, -0.2f, -1.0f);

    // locate model in world
    m_pCylinder->Translation = &cylinderTranslation;
    m_pCylinder->RotationY   =  M_PI / 1.5f;

    // create and configure pyramid
    m_pPyramid                    = new TQRConeGroup();
    m_pPyramid->OnCustomDrawItem  = OnDrawCustomStaticModelItem;
    m_pPyramid->OnLoadMeshTexture = OnLoadMeshTexture;

    std::auto_ptr<TQRColor> pPyramidColor(new TQRColor(255, 255, 255, 255));

    // load pyramid model (it's a cone with few faces)
    m_pPyramid->Load(4,
                     0.15f,
                     0.001f, // needed, otherwise the normals will not be generated correctly
                     0.001f,
                     0.12f,
                     0.12f,
                     EQR_CC_Both,
                     pPyramidColor.get(),
                     TQRModelOptions() << EQR_MO_Without_Colors);

    TQRVector3D pyramidTranslation(0.0f, -0.2f, -1.0f);

    // locate model in world
    m_pPyramid->Translation = &pyramidTranslation;

    // create and configure tetrahedron
    m_pTetrahedron                    = new TQRSphereGroup();
    m_pTetrahedron->OnCustomDrawItem  = OnDrawCustomStaticModelItem;
    m_pTetrahedron->OnLoadMeshTexture = OnLoadMeshTexture;

    std::auto_ptr<TQRColor> pTetrahedronColor(new TQRColor(255, 255, 255, 255));

    // load tetrahedron model (it's a sphere with few faces)
    m_pTetrahedron->Load(4,
                         5,
                         0.09f,
                         pTetrahedronColor.get(),
                         TQRModelOptions() << EQR_MO_Without_Colors);

    TQRVector3D tetrahedronTranslation(0.2f, -0.2f, -1.0f);

    // locate model in world
    m_pTetrahedron->Translation = &tetrahedronTranslation;
    m_pTetrahedron->RotationY   =  M_PI / 1.5f;

    // from now, OpenGL will draw scene every time the thread do nothing else
    Application->OnIdle = OnIdle;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormResize(TObject* pSender)
{
    QR_OpenGLHelper::CreateViewport(ClientWidth, ClientHeight, true);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormPaint(TObject* pSender)
{
    RenderGLScene();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::miLightingClick(TObject* pSender)
{
    // toggle light menu item
    miLighting->Checked = !miLighting->Checked;

    // enable or disable lighting
    if (miLighting->Checked)
        glEnable(GL_LIGHTING);
    else
        glDisable(GL_LIGHTING);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnIdle(TObject* pSender, bool& done)
{
    done = false;
    RenderGLScene();
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
    Draw(elapsedTime);

    glFlush();

    SwapBuffers(m_hDC);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::Draw(const double& elapsedTime)
{
    // draw models
    m_pSurface->Draw(elapsedTime);
    m_pSphere->Draw(elapsedTime);
    m_pBox->Draw(elapsedTime);
    m_pCone->Draw(elapsedTime);
    m_pTorus->Draw(elapsedTime);
    m_pParabola->Draw(elapsedTime);
    m_pCylinder->Draw(elapsedTime);
    m_pPyramid->Draw(elapsedTime);
    m_pTetrahedron->Draw(elapsedTime);
}
//--------------------------------------------------------------------------------------------------
void TMainForm::ConfigOpenGL()
{
    // configure OpenGL
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glEnable(GL_TEXTURE_2D);

    // enable lighting
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

    // set light direction (setting a position with a w value equals to 0 means a direction)
    GLfloat lightpos[] = {0.5f, 0.0f, 0.5f, 0.0f};
    glLightfv(GL_LIGHT0, GL_POSITION, lightpos);

    // set light color to white
    GLfloat color[] = {1.0f, 1.0f, 1.0f, 1.0f};
    glMaterialfv(GL_BACK, GL_DIFFUSE, color);
}
//--------------------------------------------------------------------------------------------------
GLint TMainForm::LoadTexture(int resIndex) const
{
    // load resources
    std::auto_ptr<TResourceStream> pTextureStream(new TResourceStream((int)HInstance,
                                                                      resIndex,
                                                                      PWideChar(L"DATA")));

    // load box texture
    std::auto_ptr<TBitmap> pBitmap(new TBitmap());
    pBitmap->LoadFromStream(pTextureStream.get());

    GLint index   = -1;
    BYTE* pPixels = NULL;

    try
    {
        // convert bitmap to pixel array, and create OpenGL texture from array
        QR_OpenGLHelper::BytesFromBitmap(pBitmap.get(), pPixels, false, false);
        index = QR_OpenGLHelper::CreateTexture(pBitmap->Width,
                                               pBitmap->Height,
                                               pBitmap->PixelFormat == pf32bit ? GL_RGBA : GL_RGB,
                                               pPixels,
                                               GL_NEAREST,
                                               GL_NEAREST,
                                               GL_TEXTURE_2D);
    }
    __finally
    {
        if (pPixels)
            delete[] pPixels;
    }

    return index;
}
//--------------------------------------------------------------------------------------------------
bool __fastcall TMainForm::OnLoadMeshTexture(TQRModelGroup*  pGroup,
                                             TQRModel* const pModel,
                                             TBitmap*        pBitmap,
                                             TQRTexture*     pTexture,
                                             bool&           loadNext)
{
    // no model?
    if (!pModel)
        return false;

    // no texture?
    if (!pTexture)
        return false;

    // get surface model
    TQRSurfaceModel* pSurfaceModel = dynamic_cast<TQRSurfaceModel*>(pModel);

    // found it?
    if (pSurfaceModel)
    {
        pTexture->Index = LoadTexture(ID_SURFACE_TEXTURE);
        return true;
    }

    // get sphere model
    TQRSphereModel* pSphereModel = dynamic_cast<TQRSphereModel*>(pModel);

    // found it?
    if (pSphereModel)
    {
        // select texture to create in relation with model
        if (pGroup == m_pSphere)
            pTexture->Index = LoadTexture(ID_SPHERE_TEXTURE);
        else
        if (pGroup == m_pTetrahedron)
            pTexture->Index = LoadTexture(ID_STONE_TEXTURE);

        return true;
    }

    // get box model
    TQRBoxModel* pBoxModel = dynamic_cast<TQRBoxModel*>(pModel);

    // found it?
    if (pBoxModel)
    {
        pTexture->Index = LoadTexture(ID_BOX_TEXTURE);
        return true;
    }

    // get cone model
    TQRConeModel* pConeModel = dynamic_cast<TQRConeModel*>(pModel);

    // found it?
    if (pConeModel)
    {
        // select texture to create in relation with model
        if (pGroup == m_pCone)
            pTexture->Index = LoadTexture(ID_CONE_TEXTURE);
        else
        if (pGroup == m_pCylinder)
            pTexture->Index = LoadTexture(ID_CYLINDER_TEXTURE);
        else
        if (pGroup == m_pPyramid)
            pTexture->Index = LoadTexture(ID_PYRAMID_TEXTURE);

        return true;
    }

    // get torus model
    TQRTorusModel* pTorusModel = dynamic_cast<TQRTorusModel*>(pModel);

    // found it?
    if (pTorusModel)
    {
        pTexture->Index = LoadTexture(ID_TORUS_TEXTURE);
        return true;
    }

    // get parabola model
    TQRParabolaModel* pParabolaModel = dynamic_cast<TQRParabolaModel*>(pModel);

    // found it?
    if (pParabolaModel)
    {
        pTexture->Index = LoadTexture(ID_PARABOLA_TEXTURE);
        return true;
    }

    return true;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnDrawCustomStaticModelItem(TQRModelGroup*      pGroup,
                                                       TQRModel*           pModel,
                                                       const TQRTextures   textures,
                                                       const TQRMatrix4x4& matrix)
{
    // no model?
    if (!pModel)
        return;

    // get surface model
    TQRSurfaceModel* pSurfaceModel = dynamic_cast<TQRSurfaceModel*>(pModel);

    // found it?
    if (pSurfaceModel)
    {
        TQRMesh mesh;

        // get surface mesh to draw
        pSurfaceModel->GetMesh(mesh, NULL);

        // draw mesh
        QR_OpenGLHelper::Draw(mesh, matrix, textures);

        return;
    }

    // get sphere model
    TQRSphereModel* pSphereModel = dynamic_cast<TQRSphereModel*>(pModel);

    // found it?
    if (pSphereModel)
    {
        TQRMesh mesh;

        // get sphere mesh to draw
        pSphereModel->GetMesh(mesh, NULL);

        // draw mesh
        QR_OpenGLHelper::Draw(mesh, matrix, textures);

        return;
    }

    // get box model
    TQRBoxModel* pBoxModel = dynamic_cast<TQRBoxModel*>(pModel);

    // found it?
    if (pBoxModel)
    {
        TQRMesh mesh;

        // get box mesh to draw
        pBoxModel->GetMesh(mesh, NULL);

        // draw mesh
        QR_OpenGLHelper::Draw(mesh, matrix, textures);

        return;
    }

    // get cone model
    TQRConeModel* pConeModel = dynamic_cast<TQRConeModel*>(pModel);

    // found it?
    if (pConeModel)
    {
        TQRMesh mesh;

        // get cone mesh to draw
        pConeModel->GetMesh(mesh, NULL);

        // draw mesh
        QR_OpenGLHelper::Draw(mesh, matrix, textures);

        return;
    }

    // get torus model
    TQRTorusModel* pTorusModel = dynamic_cast<TQRTorusModel*>(pModel);

    // found it?
    if (pTorusModel)
    {
        TQRMesh mesh;

        // get torus mesh to draw
        pTorusModel->GetMesh(mesh, NULL);

        // draw mesh
        QR_OpenGLHelper::Draw(mesh, matrix, textures);

        return;
    }

    // get parabola model
    TQRParabolaModel* pParabolaModel = dynamic_cast<TQRParabolaModel*>(pModel);

    // found it?
    if (pParabolaModel)
    {
        TQRMesh mesh;

        // get torus mesh to draw
        pParabolaModel->GetMesh(mesh, NULL);

        // draw mesh
        QR_OpenGLHelper::Draw(mesh, matrix, textures);

        return;
    }
}
//--------------------------------------------------------------------------------------------------
