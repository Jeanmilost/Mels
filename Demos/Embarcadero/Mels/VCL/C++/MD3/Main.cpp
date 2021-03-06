/*************************************************************************************************
 * ==> Main -------------------------------------------------------------------------------------*
 *************************************************************************************************
 * Description : MD3 demo main form                                                              *
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

// Mels library
#include <UTQRRenderer.hpp>
#include <UTQRVCLHelpers.hpp>

// engine
#include "QR_MathsHelper.h"
#include "QR_OpenGLHelper.h"

// resources
#include "Resources.rh"

#pragma package(smart_init)
#pragma link "UTQRMD3ModelGroup"
#pragma link "UTQRVCLHelpers"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
// Global defines
//--------------------------------------------------------------------------------------------------
#define GL_CLAMP_TO_EDGE 0x812F
//--------------------------------------------------------------------------------------------------
// TMainForm::IFrame
//--------------------------------------------------------------------------------------------------
TMainForm::IFrame::IFrame(bool useCollisions) :
    m_pMesh(NULL),
    m_pAABBTree(NULL)
{
    m_pMesh = new TQRMesh();

    if (useCollisions)
        m_pAABBTree = new TQRAABBTree();
}
//--------------------------------------------------------------------------------------------------
TMainForm::IFrame::~IFrame()
{
    if (m_pMesh)
        delete m_pMesh;

    if (m_pAABBTree)
        delete m_pAABBTree;
}
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_pOptions(NULL),
    m_hDC(NULL),
    m_hRC(NULL),
    m_pMD3(NULL),
    m_pTextureShader(NULL),
    m_pColorShader(NULL),
    m_PreviousTime(::GetTickCount()),
    m_CurTorsoGesture(0),
    m_CurLegsGesture(0),
    m_AnimCached(false),
    m_Cached(false)
{}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    // delete cached frames, if any
    for (ICache::iterator itCache = m_Cache.begin(); itCache != m_Cache.end(); ++itCache)
    {
        for (IFrames::iterator itFrames = itCache->second->begin(); itFrames != itCache->second->end();
                ++itFrames)
            delete itFrames->second;

        itCache->second->clear();
        delete itCache->second;
    }

    m_Cache.clear();

    // delete texture shader
    if (m_pTextureShader)
        delete m_pTextureShader;

    // delete color shader
    if (m_pColorShader)
        delete m_pColorShader;

    // delete MD3 model
    if (m_pMD3)
        delete m_pMD3;

    // shutdown OpenGL
    QR_OpenGLHelper::DisableOpenGL(Handle, m_hDC, m_hRC);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject* pSender)
{
    // create and show options to user
    m_pOptions = new TOptions(this);
    m_pOptions->ShowModal();

    if (m_pOptions->IsAppClosing())
        return;

    // do show model in full screen?
    if (m_pOptions->ckFullScreen->Checked)
    {
        BorderStyle = bsNone;
        WindowState = wsMaximized;
        ::ShowCursor(m_pOptions->ckShowCollisions->Checked && m_pOptions->rgCacheOptions->ItemIndex != 1);
    }
    else
    {
        BorderStyle = bsSizeable;
        WindowState = wsNormal;
        ::ShowCursor(true);
    }

    BringToFront();

    // select correct cursor to use
    paRendering->Cursor = (m_pOptions->ckShowCollisions->Checked ? crCross : crDefault);

    // initialize OpenGL
    if (!QR_OpenGLHelper::EnableOpenGL(paRendering->Handle, m_hDC, m_hRC))
    {
        MessageDlg("OpenGL could not be initialized.\r\n\r\nApplication will close.", mtError,
                TMsgDlgButtons() << mbOK, 0);
        Application->Terminate();
        return;
    }

    // do use shader?
    if (m_pOptions->ckUseShader->Checked)
    {
        // stop GLEW crashing on OSX :-/
        glewExperimental = GL_TRUE;

        // initialize GLEW
        if (glewInit() != GLEW_OK)
        {
            MessageDlg("Could not initialize GLEW library.\r\n\r\nApplication will close.", mtError,
                    TMsgDlgButtons() << mbOK, 0);
            Application->Terminate();
            return;
        }
    }

    // configure OpenGL
    ConfigOpenGL();

    // load MD3 model
    if (!LoadModel())
    {
        MessageDlg("Failed to load MD3 model.\r\n\r\nApplication will close.", mtError,
                TMsgDlgButtons() << mbOK, 0);
        Application->Terminate();
        return;
    }

    // from now, OpenGL will draw scene every time the thread do nothing else
    Application->OnIdle = OnIdle;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormResize(TObject* pSender)
{
    const float   fov    = 45.0f;
    const float   zNear  = 1.0f;
    const float   zFar   = 200.0f;
    const GLfloat aspect = (GLfloat)ClientWidth / (ClientHeight ? (GLfloat)ClientHeight : 1.0f);

    // create projection matrix (will not be modified while execution)
    m_ProjectionMatrix = TQRRenderer::GetPerspective(fov,
                                                     aspect,
                                                     zNear,
                                                     zFar,
                                                     m_pOptions->ckUseOrthoMatrix->Checked);

    TQRVector3D position(0.0f, 0.0f, 0.0f);
    TQRVector3D direction(0.0f, 0.0f, 1.0f);
    TQRVector3D up(0.0f, 1.0f, 0.0f);

    // create view matrix (will not be modified while execution)
    m_ViewMatrix = TQRRenderer::LookAtLH(position, direction, up);

    QR_OpenGLHelper::CreateViewport(ClientWidth,
                                    ClientHeight,
                                    !m_pOptions->ckUseShader->Checked &&
                                    !m_pOptions->ckUseOrthoMatrix->Checked);

    // configure matrices for OpenGL direct mode
    if (m_pOptions->ckUseOrthoMatrix->Checked)
    {
        const float maxY = zNear * std::tanf(fov * M_PI / 360.0);
        const float maxX = maxY  * aspect;

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glOrtho(-maxX, maxX, -maxY, maxY, zNear, zFar);
        glMatrixMode(GL_MODELVIEW);
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormKeyPress(TObject* pSender, WideChar& key)
{
    switch (key)
    {
        case VK_ESCAPE:
            Application->Terminate();
            break;
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormPaint(TObject* pSender)
{
    RenderGLScene();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::miPrevTorsoAnimClick(TObject* pSender)
{
    --m_CurTorsoGesture;

    if (m_CurTorsoGesture < EQR_AG_MD3_Both_Death1)
        m_CurTorsoGesture = EQR_AG_MD3_Torso_Stand2;

    // set new torso animation gesture
    m_pMD3->SetAnimation(L"upper", (EQRMD3AnimationGesture)m_CurTorsoGesture);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::miNextTorsoAnimClick(TObject* pSender)
{
    ++m_CurTorsoGesture;

    if (m_CurTorsoGesture > EQR_AG_MD3_Torso_Stand2)
        m_CurTorsoGesture = EQR_AG_MD3_Both_Death1;

    // set new torso animation gesture
    m_pMD3->SetAnimation(L"upper", (EQRMD3AnimationGesture)m_CurTorsoGesture);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::miPrevLegsAnimClick(TObject* pSender)
{
    --m_CurLegsGesture;

    if (m_CurLegsGesture < EQR_AG_MD3_Both_Death1)
        m_CurLegsGesture = EQR_AG_MD3_Legs_Turn;

    if (m_CurLegsGesture > EQR_AG_MD3_Both_Dead3 && m_CurLegsGesture < EQR_AG_MD3_Legs_Walk_Crouching)
        m_CurLegsGesture = EQR_AG_MD3_Both_Dead3;

    // set new legs animation gesture
    m_pMD3->SetAnimation(L"lower", (EQRMD3AnimationGesture)m_CurLegsGesture);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::miNextLegsAnimClick(TObject* pSender)
{
    ++m_CurLegsGesture;

    if (m_CurLegsGesture > EQR_AG_MD3_Legs_Turn)
        m_CurLegsGesture = EQR_AG_MD3_Both_Death1;

    if (m_CurLegsGesture > EQR_AG_MD3_Both_Dead3 && m_CurLegsGesture < EQR_AG_MD3_Legs_Walk_Crouching)
        m_CurLegsGesture = EQR_AG_MD3_Legs_Walk_Crouching;

    // set new legs animation gesture
    m_pMD3->SetAnimation(L"lower", (EQRMD3AnimationGesture)m_CurLegsGesture);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnIdle(TObject* pSender, bool& done)
{
    done = false;
    RenderGLScene();
    UpdateCacheProgress();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::RenderGLScene()
{
    // calculate time interval
    const std::time_t now            = ::GetTickCount();
    const double      elapsedTime    = (now - m_PreviousTime);
                      m_PreviousTime = now;

    if (!m_hDC || !m_hRC)
        return;

    wglMakeCurrent(m_hDC, m_hRC);

    // clear scene
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // draw scene
    Draw(elapsedTime);

    glFlush();

    ::SwapBuffers(m_hDC);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::Draw(const double& elapsedTime)
{
    // draw model
    m_pMD3->Draw(elapsedTime);
}
//--------------------------------------------------------------------------------------------------
void TMainForm::ConfigOpenGL()
{
    // configure OpenGL
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);
    glEnable(GL_TEXTURE_2D);
}
//--------------------------------------------------------------------------------------------------
bool TMainForm::BuildShader(TStream* pVertexPrg, TStream* pFragmentPrg, QR_Shader_OpenGL* pShader)
{
    // load and compile shader
    pShader->CreateProgram();
    pShader->AttachFile(pVertexPrg,   EQR_ST_Vertex);
    pShader->AttachFile(pFragmentPrg, EQR_ST_Fragment);

    // try to link shader
    return pShader->Link(false);
}
//--------------------------------------------------------------------------------------------------
bool TMainForm::LoadModel()
{
    // delete cached frames, if any
    for (ICache::iterator itCache = m_Cache.begin(); itCache != m_Cache.end(); ++itCache)
    {
        for (IFrames::iterator itFrames = itCache->second->begin(); itFrames != itCache->second->end();
                ++itFrames)
            delete itFrames->second;

        itCache->second->clear();
        delete itCache->second;
    }

    m_Cache.clear();

    // do use shader?
    if (m_pOptions->ckUseShader->Checked)
    {
        // color shader still not loaded?
        if (!m_pColorShader)
        {
            // load shader programs from resource
            std::auto_ptr<TResourceStream> pVertexPrg(new TResourceStream((int)HInstance,
                                                                          ID_COLOR_VERTEX_SHADER,
                                                                          PWideChar(L"DATA")));
            std::auto_ptr<TResourceStream> pFragmentPrg(new TResourceStream((int)HInstance,
                                                                            ID_COLOR_FRAGMENT_SHADER,
                                                                            PWideChar(L"DATA")));

            // create color shader
            m_pColorShader = new QR_Shader_OpenGL();

            // try to build shader
            if (!BuildShader(pVertexPrg.get(), pFragmentPrg.get(), m_pColorShader))
                return false;
        }

        // texture shader still not loaded?
        if (!m_pTextureShader)
        {
            // load shader programs from resource
            std::auto_ptr<TResourceStream> pVertexPrg(new TResourceStream((int)HInstance,
                                                                          ID_TEXTURE_VERTEX_SHADER,
                                                                          PWideChar(L"DATA")));
            std::auto_ptr<TResourceStream> pFragmentPrg(new TResourceStream((int)HInstance,
                                                                            ID_TEXTURE_FRAGMENT_SHADER,
                                                                            PWideChar(L"DATA")));

            // create texture shader
            m_pTextureShader = new QR_Shader_OpenGL();

            // try to build shader
            if (!BuildShader(pVertexPrg.get(), pFragmentPrg.get(), m_pTextureShader))
                return false;
        }
    }

    // create MD3 model, if needed
    if (!m_pMD3)
    {
        m_pMD3 = new TQRMD3Group();

        m_pMD3->OnLoadMeshTexture = OnLoadMeshTexture;
        m_pMD3->OnDrawItem        = OnDrawModelItem;
        m_pMD3->OnCustomDrawItem  = OnDrawCustomModelItem;
    }

    TQRModelOptions       modelOptions;
    TQRFramedModelOptions framedModelOptions;

    // set basic configuration (normals are not required here as the lighting is not used for now)
    modelOptions << EQR_MO_Without_Normals;

    // dispatch caching type
    switch (m_pOptions->rgCacheOptions->ItemIndex)
    {
        case 0: modelOptions << EQR_MO_Create_Cache;            break;
        case 1: modelOptions << EQR_MO_Dynamic_Frames_No_Cache; break;
        case 2: modelOptions << EQR_MO_Dynamic_Frames;          break;
        case 3:                                                 break;
        default:                                                return false;
    }

    // if shader is used, interpolation will be done on the shader side
    if (!m_pOptions->ckUseShader->Checked)
        framedModelOptions << EQR_FO_Interpolate;

    std::auto_ptr<TQRColor> pColor(new TQRColor(255, 255, 255, 255));

    // do load custom MD3 model?
    if (!m_pOptions->edModelFileName->Text.IsEmpty())
    {
        // open package stream
        std::auto_ptr<TFileStream> pFileStream(new TFileStream(m_pOptions->edModelFileName->Text,
                                                               fmOpenRead));

        if (!m_pMD3->Load(pFileStream.get(),
                          pColor.get(),
                          false,
                          modelOptions,
                          framedModelOptions))
            return false;

        pFileStream.release();
    }
    else
    {
        // load resources
        std::auto_ptr<TResourceStream> pModelStream(new TResourceStream((int)HInstance,
                                                                        ID_MD3_MODEL,
                                                                        PWideChar(L"DATA")));

        // load model from resources
        if (!m_pMD3->Load(pModelStream.get(),
                          pColor.get(),
                          false,
                          modelOptions,
                          framedModelOptions,
                          m_pOptions->GetSelectedTeam()))
            return false;

        pModelStream.release();
    }

    // orthogonal matrix is used?
    if (m_pOptions->ckUseOrthoMatrix->Checked)
    {
        // translate and scale model
        *m_pMD3->Translation = TQRVector3D(0.0f,   -0.06f,  -1.5f);
        *m_pMD3->Scaling     = TQRVector3D(0.011f,  0.011f,  0.011f);
    }
    else
    {
        // translate and scale model
        *m_pMD3->Translation = TQRVector3D(0.0f,   -0.1f,   -1.5f);
        *m_pMD3->Scaling     = TQRVector3D(0.015f,  0.015f,  0.015f);
    }

    // rotate model
     m_pMD3->RotationX = -M_PI_2; // -90�
     m_pMD3->RotationY = -M_PI_4; // -45�

    m_CurTorsoGesture = EQR_AG_MD3_Torso_Stand;
    m_CurLegsGesture  = EQR_AG_MD3_Legs_Walk;

    // set default animation gesture
    m_pMD3->SetAnimation(L"upper", (EQRMD3AnimationGesture)m_CurTorsoGesture);
    m_pMD3->SetAnimation(L"lower", (EQRMD3AnimationGesture)m_CurLegsGesture);

    return true;
}
//--------------------------------------------------------------------------------------------------
void TMainForm::UpdateCacheProgress()
{
    // get job status
    TQRModelJobStatus* pJobStatus = m_pMD3->QueryJobStatus();

    // found it?
    if (!pJobStatus)
    {
        pbLoadModel->Visible = false;
        return;
    }

    // job was termined?
    if (pJobStatus->Status == EQR_JS_Done || pJobStatus->Status == EQR_JS_Error)
    {
        pbLoadModel->Visible = false;
        return;
    }

    // show job progress
    pbLoadModel->Visible  = true;
    pbLoadModel->Max      = 100;
    pbLoadModel->Position = pJobStatus->Progress;
}
//--------------------------------------------------------------------------------------------------
TMainForm::IFrame* TMainForm::GetFrame(std::size_t index, TQRMD3Model* pModel, bool useCollision)
{
    // search for model in cache (a md3 contains many sub-models)
    ICache::iterator itCache = m_Cache.find(pModel);

    // found it?
    if (itCache == m_Cache.end())
    {
        // create new frame item, and populate it
        std::auto_ptr<IFrame> pFrame(new IFrame(useCollision));
        pModel->GetMesh(index, *pFrame->m_pMesh, pFrame->m_pAABBTree);

        // create new model item, and populate it
        std::auto_ptr<IFrames> pFrames(new IFrames());
        (*pFrames)[index]    = pFrame.get();
        IFrame* pCachedFrame = pFrame.release();

        // cache current model frame
        m_Cache[pModel] = pFrames.get();
        pFrames.release();

        return pCachedFrame;
    }

    // search for cached frame in model cache
    IFrames::iterator itFrame = itCache->second->find(index);

    // found it?
    if (itFrame != itCache->second->end())
        return itFrame->second;

    // create new frame item, populate it, and add it to cache
    std::auto_ptr<IFrame> pFrame(new IFrame(useCollision));
    pModel->GetMesh(index, *pFrame->m_pMesh, pFrame->m_pAABBTree);
    (*itCache->second)[index] = pFrame.get();

    return pFrame.release();
}
//--------------------------------------------------------------------------------------------------
void TMainForm::DetectAndDrawCollisions(const TQRMatrix4x4& modelMatrix, TQRAABBTree* pAABBTree)
{
    if (!m_pOptions->ckShowCollisions->Checked || !pAABBTree)
        return;

    // calculate client rect in OpenGL coordinates
    TQRRect rect(-1.0f, 1.0f, 2.0f, 2.0f);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    TQRVector3D rayPos = QR_OpenGLHelper::MousePosToGLPoint(Handle, rect);
    TQRVector3D rayDir;

    // orthogonal matrix is used?
    if (!m_pOptions->ckUseOrthoMatrix->Checked)
        rayDir = TQRVector3D(rayPos.X, rayPos.Y, 1.0f);
    else
        rayDir = TQRVector3D(0.0f, 0.0f, 1.0f);

    // unproject the ray to make it inside the 3d world coordinates
    TQRRenderer::Unproject(m_ProjectionMatrix, m_ViewMatrix, rayPos, rayDir);

    float determinant;

    // now transform the ray to match with the model position
    TQRMatrix4x4 invertModel = const_cast<TQRMatrix4x4&>(modelMatrix).Inverse(determinant);
    rayPos                   = invertModel.Transform(rayPos);
    rayDir                   = invertModel.TransformNormal(rayDir);
    rayDir                   = rayDir.Normalize();

    // create and populate ray from mouse position
    std::auto_ptr<TQRRay> pRay(new TQRRay());
    pRay->Pos = &rayPos;
    pRay->Dir = &rayDir;

    TQRPolygons polygons;

    // get polygons to check for collision by resolving AABB tree
    pAABBTree->Resolve(pRay.get(), polygons);

    const std::size_t polygonCount = polygons.Length;
          TQRPolygons polygonToDraw;

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

    // found polgons to draw?
    if (!polygonToDrawCount)
        return;

    TQRMesh mesh;
    mesh.Length = 1;

    //TQRVertex vertex;
    mesh[0].m_Type          = EQR_VT_Triangles;
    mesh[0].m_CoordType     = EQR_VC_XYZ;
    mesh[0].m_Stride        = 7;
    mesh[0].m_Buffer.Length = polygonToDrawCount * (mesh[0].m_Stride * 3);
    mesh[0].m_Format << EQR_VF_Colors;

    std::size_t offset = 0;

    // iterate through polygons to draw
    for (std::size_t i = 0; i < polygonToDrawCount; ++i)
    {
        // build polygon to show
        mesh[0].m_Buffer[offset]      = polygonToDraw[i].Vertex1->X;
        mesh[0].m_Buffer[offset + 1]  = polygonToDraw[i].Vertex1->Y;
        mesh[0].m_Buffer[offset + 2]  = polygonToDraw[i].Vertex1->Z;
        mesh[0].m_Buffer[offset + 3]  = 1.0f;
        mesh[0].m_Buffer[offset + 4]  = 0.0f;
        mesh[0].m_Buffer[offset + 5]  = 0.0f;
        mesh[0].m_Buffer[offset + 6]  = 1.0f;
        mesh[0].m_Buffer[offset + 7]  = polygonToDraw[i].Vertex2->X;
        mesh[0].m_Buffer[offset + 8]  = polygonToDraw[i].Vertex2->Y;
        mesh[0].m_Buffer[offset + 9]  = polygonToDraw[i].Vertex2->Z;
        mesh[0].m_Buffer[offset + 10] = 0.8f;
        mesh[0].m_Buffer[offset + 11] = 0.0f;
        mesh[0].m_Buffer[offset + 12] = 0.2f;
        mesh[0].m_Buffer[offset + 13] = 1.0f;
        mesh[0].m_Buffer[offset + 14] = polygonToDraw[i].Vertex3->X;
        mesh[0].m_Buffer[offset + 15] = polygonToDraw[i].Vertex3->Y;
        mesh[0].m_Buffer[offset + 16] = polygonToDraw[i].Vertex3->Z;
        mesh[0].m_Buffer[offset + 17] = 1.0f;
        mesh[0].m_Buffer[offset + 18] = 0.12f;
        mesh[0].m_Buffer[offset + 19] = 0.2f;
        mesh[0].m_Buffer[offset + 20] = 1.0f;

        // go to next polygon
        offset += (mesh[0].m_Stride * 3);
    }

    // do use shader?
    if (m_pOptions->ckUseShader->Checked)
    {
        // bind shader program
        m_pColorShader->Use(true);

        // get perspective (or projection) matrix slot from shader
        GLint uniform = QR_OpenGLHelper::GetUniform(m_pColorShader, EQR_SA_PerspectiveMatrix);

        // found it?
        if (uniform == -1)
            throw "Program uniform not found - perspective";

        // connect perspective (or projection) matrix to shader
        glUniformMatrix4fv(uniform, 1, GL_FALSE, m_ProjectionMatrix.GetPtr());

        // get view (or camera) matrix slot from shader
        uniform = QR_OpenGLHelper::GetUniform(m_pColorShader, EQR_SA_CameraMatrix);

        // found it?
        if (uniform == -1)
            throw "Program uniform not found - camera";

        // connect view (or camera) matrix to shader
        glUniformMatrix4fv(uniform, 1, GL_FALSE, m_ViewMatrix.GetPtr());

        // unbind shader program
        m_pColorShader->Use(false);

        // configure OpenGL to draw polygons in collision
        glDisable(GL_TEXTURE_2D);
        glCullFace(GL_NONE);
        glDisable(GL_DEPTH_TEST);

        TQRTextures textures;

        // draw mesh
        QR_OpenGLHelper::Draw(mesh, modelMatrix, textures, m_pColorShader);

        // restore previous OpenGL parameters
        glEnable(GL_DEPTH_TEST);
        glCullFace(GL_FRONT);
        glEnable(GL_TEXTURE_2D);

        glFlush();
    }
    else
    {
        glMatrixMode(GL_MODELVIEW);

        glPushMatrix();

        // unfortunately, because Delphi don't allow to declare a const function as in C++
        TQRMatrix4x4* pModelMatrix = const_cast<TQRMatrix4x4*>(&modelMatrix);

        // place triangles into 3D world
        glLoadMatrixf(pModelMatrix->GetPtr());

        // configure OpenGL to draw polygons in collision
        glDisable(GL_TEXTURE_2D);
        glCullFace(GL_NONE);
        glDisable(GL_DEPTH_TEST);

        TQRTextures textures;

        // draw polygons in collision with mouse pointer
        QR_OpenGLHelper::Draw(mesh, modelMatrix, textures);

        // restore previous OpenGL parameters
        glEnable(GL_DEPTH_TEST);
        glCullFace(GL_FRONT);
        glEnable(GL_TEXTURE_2D);

        glPopMatrix();

        glFlush();
    }
}
//--------------------------------------------------------------------------------------------------
void TMainForm::PrepareShaderToDrawModel(      QR_Shader_OpenGL* pShader,
                                         const UnicodeString&    modelName,
                                         const TQRTextures&      textures)
{
    if (!pShader)
        return;

    // bind shader program
    pShader->Use(true);

    // get perspective (or projection) matrix slot from shader
    GLint uniform = QR_OpenGLHelper::GetUniform(pShader, EQR_SA_PerspectiveMatrix);

    // found it?
    if (uniform == -1)
        throw "Program uniform not found - perspective";

    // connect perspective (or projection) matrix to shader
    glUniformMatrix4fv(uniform, 1, GL_FALSE, m_ProjectionMatrix.GetPtr());

    // get view (or camera) matrix slot from shader
    uniform = QR_OpenGLHelper::GetUniform(pShader, EQR_SA_CameraMatrix);

    // found it?
    if (uniform == -1)
        throw "Program uniform not found - camera";

    // connect view (or camera) matrix to shader
    glUniformMatrix4fv(uniform, 1, GL_FALSE, m_ViewMatrix.GetPtr());

    // unbind shader program
    pShader->Use(false);
}
//--------------------------------------------------------------------------------------------------
bool __fastcall TMainForm::OnLoadMeshTexture(TQRModelGroup* const pGroup,
                                             TQRModel* const      pModel,
                                             TBitmap*             pBitmap,
                                             TQRTexture*          pTexture,
                                             bool&                loadNext)
{
    if (!pModel)
        return false;

    if (!pTexture)
        return false;

    if (!pBitmap)
    {
        pTexture->Enabled = false;
        return true;
    }

    // make sure texture is a power of 2 texture (OpenGL may not support non POT textures)
    TQRModelGroupHelper::MakeTexturePowerOf2(pBitmap, pBitmap);

    BYTE* pPixels = NULL;

    try
    {
        // convert bitmap to pixel array, and create OpenGL texture from array
        QR_OpenGLHelper::BytesFromBitmap(pBitmap, pPixels, false, false);
        pTexture->Index = QR_OpenGLHelper::CreateTexture(pBitmap->Width,
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

    return true;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnDrawModelItem(TQRModelGroup* const pGroup,
                                           TQRModel* const      pModel,
                                           const TQRTextures    textures,
                                           const TQRMatrix4x4&  matrix,
                                           NativeInt            index,
                                           NativeInt            nextIndex,
                                           const double         interpolationFactor,
                                           const PQRMesh        pMesh,
                                           const PQRMesh        pNextMesh,
                                           TQRAABBTree* const   pTree,
                                           TQRAABBTree* const   pNextTree)
{
    if (!pModel)
        return;

    if (!pMesh)
        return;

    PQRMesh pNextMeshToDraw;

    if (!pNextMesh)
        pNextMeshToDraw = pMesh;
    else
        pNextMeshToDraw = pNextMesh;

    // do use shader?
    if (m_pOptions->ckUseShader->Checked)
    {
        // prepare shader to draw the model
        PrepareShaderToDrawModel(m_pTextureShader, L"", textures);

        // draw mesh
        QR_OpenGLHelper::Draw(*pMesh,
                              *pNextMeshToDraw,
                               matrix,
                               interpolationFactor,
                               textures,
                               m_pTextureShader);
    }
    else
    {
        TQRMesh mesh;

        // get next frame to draw
        TQRModelHelper::Interpolate(interpolationFactor, *pMesh, *pNextMeshToDraw, mesh);

        // draw mesh
        QR_OpenGLHelper::Draw(mesh, matrix, textures);
    }

    DetectAndDrawCollisions(matrix, pTree);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnDrawCustomModelItem(TQRModelGroup* const pGroup,
                                                 TQRModel*            pModel,
                                                 const TQRTextures    textures,
                                                 const TQRMatrix4x4&  matrix,
                                                 NativeInt            index,
                                                 NativeInt            nextIndex,
                                                 const double         interpolationFactor)
{
    // no model to draw?
    if (!pModel)
        return;

    // get MD3 model
    TQRMD3Model* pMD3Model = static_cast<TQRMD3Model*>(pModel);

    // found it?
    if (!pMD3Model)
        return;

    // get mesh count
    const std::size_t meshCount = pMD3Model->GetMeshCount();

    // are indexes out of bounds?
    if (index > (NativeInt)meshCount || nextIndex > (NativeInt)meshCount)
        return;

    TQRMesh* pMeshToDraw;
    TQRMesh* pNextMeshToDraw = NULL;
    bool     interpolated    = false;

    // get frame to draw, and frame to interpolate with
    IFrame* pFrame     = GetFrame(index,     pMD3Model, m_pOptions->ckShowCollisions);
    IFrame* pNextFrame = GetFrame(nextIndex, pMD3Model, m_pOptions->ckShowCollisions);

    // do use shader?
    if (!m_pOptions->ckUseShader->Checked)
    {
        pMeshToDraw = new TQRMesh();

        // interpolate and get next mesh to draw
        TQRModelHelper::Interpolate(interpolationFactor,
                                    *pFrame->m_pMesh,
                                    *pNextFrame->m_pMesh,
                                    *pMeshToDraw);

        interpolated = true;
    }
    else
    {
        // get meshes to send to shader
        pMeshToDraw     = pFrame->m_pMesh;
        pNextMeshToDraw = pNextFrame->m_pMesh;
    }

    // get aligned-axis bounding box tree to use to detect collisions
    TQRAABBTree* pAABBTree = pFrame->m_pAABBTree;

    // do use shader?
    if (m_pOptions->ckUseShader->Checked)
    {
        // do interpolate meshes on the shader side?
        if (!interpolated && pNextMeshToDraw)
        {
            // prepare shader to draw the model
            PrepareShaderToDrawModel(m_pTextureShader, L"", textures);

            // draw mesh
            QR_OpenGLHelper::Draw(*pMeshToDraw,
                                  *pNextMeshToDraw,
                                   matrix,
                                   interpolationFactor,
                                   textures,
                                   m_pTextureShader);
        }
        else
        {
            // prepare shader to draw the model
            PrepareShaderToDrawModel(m_pTextureShader, L"", textures);

            // draw mesh
            QR_OpenGLHelper::Draw(*pMeshToDraw, matrix, textures, m_pTextureShader);
        }
    }
    else
        // draw mesh
        QR_OpenGLHelper::Draw(*pMeshToDraw, matrix, textures);

    if (interpolated)
        delete pMeshToDraw;

    DetectAndDrawCollisions(matrix, pAABBTree);
}
//--------------------------------------------------------------------------------------------------
