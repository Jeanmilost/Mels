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

// vcl
#include <Vcl.Graphics.hpp>

// std
#include <memory>
#include <string>

// engine
#include "QR_MathsHelper.h"
#include "QR_OpenGLHelper.h"

// interface
#include "TOptions.h"

// resources
#include "Resources.rh"

#pragma package(smart_init)
#pragma link "UTQRMD3"
#pragma resource "*.dfm"

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
// TMainForm::ITextureItem
//--------------------------------------------------------------------------------------------------
TMainForm::ITextureItem::ITextureItem() :
    m_ID(0)
{}
//--------------------------------------------------------------------------------------------------
TMainForm::ITextureItem::ITextureItem(std::size_t id, const std::wstring& name) :
    m_ID(id),
    m_Name(name)
{}
//--------------------------------------------------------------------------------------------------
TMainForm::ITextureItem::~ITextureItem()
{}
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_hDC(NULL),
    m_hRC(NULL),
    m_pMD3(NULL),
    m_pColorShader(NULL),
    m_pTextureShader(NULL),
    m_PreviousTime(::GetTickCount()),
    m_InterpolationFactor(0.0),
    m_FrameIndex(0),
    m_FullScreen(false),
    m_UseShader(true),
    m_Collisions(true)
{}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    // delete textures
    for (int i = 0; i < m_Textures.Length; ++i)
        delete m_Textures[i];

    // delete cached frames, if any
    for (IFrames::iterator it = m_Frames.begin(); it != m_Frames.end(); ++it)
        delete it->second;

    m_Frames.clear();

    // delete color shader
    if (m_pColorShader)
        delete m_pColorShader;

    // delete texture shader
    if (m_pTextureShader)
        delete m_pTextureShader;

    // delete MD3 model
    if (m_pMD3)
        delete m_pMD3;

    // shutdown OpenGL
    QR_OpenGLHelper::DisableOpenGL(Handle, m_hDC, m_hRC);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject* pSender)
{
    // show options
    std::auto_ptr<TOptions> pOptions(new TOptions(this));
    pOptions->ckFullScreen->Checked = m_FullScreen;
    pOptions->ckUseShader->Checked  = m_UseShader;
    pOptions->ckCollisions->Checked = m_Collisions;
    pOptions->ShowModal();

    // apply options
    m_FullScreen = pOptions->ckFullScreen->Checked;
    m_UseShader  = pOptions->ckUseShader->Checked;
    m_Collisions = pOptions->ckCollisions->Checked;

    // do show model in full screen?
    if (m_FullScreen)
    {
        BorderStyle = bsNone;
        WindowState = wsMaximized;
        ::ShowCursor(m_Collisions);
    }
    else
    {
        BorderStyle = bsSizeable;
        WindowState = wsNormal;
        ::ShowCursor(true);
    }

    BringToFront();

    // select correct cursor to use
    paRendering->Cursor = m_Collisions ? crCross : crDefault;

    // initialize OpenGL
    if (!QR_OpenGLHelper::EnableOpenGL(paRendering->Handle, m_hDC, m_hRC))
    {
        MessageDlg("OpenGL could not be initialized.\r\n\r\nApplication will close.", mtError,
                TMsgDlgButtons() << mbOK, 0);
        Application->Terminate();
        return;
    }

    // do use shader?
    if (m_UseShader)
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
    if (!LoadModel(m_UseShader))
    {
        MessageDlg("Failed to load MD3 model.\r\n\r\nApplication will close.", mtError,
                TMsgDlgButtons() << mbOK, 0);
        Application->Terminate();
        return;
    }

    // from now, OpenGL will draw scene every time the thread do nothing else
    Application->OnIdle = IdleLoop;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormResize(TObject* pSender)
{
    // create projection matrix (will not be modified while execution)
    m_ProjectionMatrix = QR_OpenGLHelper::GetOrtho(-1.0f,
                                                    1.0f,
                                                   -1.0f,
                                                    1.0f,
                                                   -100.0f,
                                                    100.0f);

    TQRVector3D position(0.0f, 0.0f, 0.0f);
    TQRVector3D direction(0.0f, 0.0f, 1.0f);
    TQRVector3D up(0.0f, 1.0f, 0.0f);

    // create view matrix (will not be modified while execution)
    m_ViewMatrix = QR_OpenGLHelper::LookAtLH(position, direction, up);

    // create viewport
    QR_OpenGLHelper::CreateViewport(ClientWidth, ClientHeight, !m_UseShader);

    // configure matrices for OpenGL direct mode
    if (!m_UseShader)
    {
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glOrtho(-1.0f, 1.0f, -1.0f, 1.0f, -100.0f, 100.0f);
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
void __fastcall TMainForm::IdleLoop(TObject* pSender, bool& done)
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
                      m_PreviousTime = now;

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
    DrawModel(m_pMD3,
              m_Textures,
              m_ModelMatrix,
              0,
              0,
              m_InterpolationFactor,
              m_UseShader,
              m_Collisions);
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
bool TMainForm::LoadModel(bool useShader)
{
    // delete cached frames, if any
    for (IFrames::iterator it = m_Frames.begin(); it != m_Frames.end(); ++it)
        delete it->second;

    m_Frames.clear();

    // do use shader?
    if (useShader)
    {
        // color shader still not loaded?
        if (!m_pColorShader)
        {
            // load shader programs from resource
            std::auto_ptr<TResourceStream> pVertexPrg(new TResourceStream((int)HInstance,
                                                                          ID_COLOR_VERTEX_SHADER,
                                                                          L"DATA"));
            std::auto_ptr<TResourceStream> pFragmentPrg(new TResourceStream((int)HInstance,
                                                                            ID_COLOR_FRAGMENT_SHADER,
                                                                            L"DATA"));

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
                                                                          L"DATA"));
            std::auto_ptr<TResourceStream> pFragmentPrg(new TResourceStream((int)HInstance,
                                                                            ID_TEXTURE_FRAGMENT_SHADER,
                                                                            L"DATA"));

            // create texture shader
            m_pTextureShader = new QR_Shader_OpenGL();

            // try to build shader
            if (!BuildShader(pVertexPrg.get(), pFragmentPrg.get(), m_pTextureShader))
                return false;
        }
    }

    // create MD3 model, if needed
    if (!m_pMD3)
        m_pMD3 = new TQRMD3Model();

    // load resources
    std::auto_ptr<TResourceStream> pModelStream(new TResourceStream((int)HInstance,
                                                                    ID_MD3_MODEL,
                                                                    L"DATA"));

    // load model
    if (!m_pMD3->Load(pModelStream.get(), pModelStream->Size))
        return false;

    m_pMD3->VertexFormat = TQRVertexFormat() << EQR_VF_TexCoords << EQR_VF_Colors;

    // create model matrix
    m_ModelMatrix = TQRMatrix4x4::Identity();
    m_ModelMatrix.Translate(TQRVector3D(0.0f, -0.6f, -1.0f));
    m_ModelMatrix.Rotate(-M_PI_4, TQRVector3D(1.0f, 0.0f, 0.0f)); // -45°
    m_ModelMatrix.Rotate(-M_PI_4, TQRVector3D(0.0f, 0.0f, 1.0f)); // -45°
    m_ModelMatrix.Scale(TQRVector3D(0.02f, 0.02f, 0.02f));

    // link texture to use with model parts
    ITextureTable textureTable;
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_GLASS_PANE, L"lower_glass"));
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_CLOCK_FACE, L"clock_face"));
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_GLASS_PANE, L"clock_glass"));
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_GF_CLOCK,   L"feet"));
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_GF_CLOCK,   L"pendulam"));
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_SHAND,      L"small_hand"));
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_BHAND,      L"big_hand"));
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_GF_CLOCK,   L"balance"));
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_GF_CLOCK,   L"clock_body"));
    textureTable.push_back(ITextureItem(ID_MD3_TEXTURE_GF_CLOCK,   L"clock_face_top"));

    // load all textures
    LoadTexture(&m_Textures, textureTable);

    return true;
}
//--------------------------------------------------------------------------------------------------
TMainForm::IFrame* TMainForm::GetFrame(std::size_t index, TQRMD3Model* pModel, bool useCollision)
{
    IFrames::iterator it = m_Frames.find(index);

    if (it != m_Frames.end())
        return it->second;

    std::auto_ptr<IFrame> pFrame(new IFrame(useCollision));
    pModel->GetMesh(index, *pFrame->m_pMesh, pFrame->m_pAABBTree);
    m_Frames[index] = pFrame.get();

    return pFrame.release();
}
//--------------------------------------------------------------------------------------------------
void TMainForm::DetectAndDrawCollisions(const TQRMatrix4x4& modelMatrix,
                                              TQRAABBTree*  pAABBTree,
                                              bool          useShader,
                                              bool          collisions)
{
    if (!collisions || !pAABBTree)
        return;

    // calculate client rect in OpenGL coordinates
    TQRRect rect(-1.0, 1.0, 2.0, 2.0);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    TQRVector3D rayPos = QR_OpenGLHelper::MousePosToGLPoint(Handle, rect);
    TQRVector3D rayDir = TQRVector3D(0.0, 0.0, 1.0);

    float determinant;

    // transform the ray to be on the same coordinates system as the model
    TQRMatrix4x4 invertMatrix =
            const_cast<TQRMatrix4x4&>(modelMatrix).Multiply(m_ViewMatrix).Multiply(m_ProjectionMatrix).Inverse(determinant);
    rayPos       = invertMatrix.Transform(rayPos);
    rayDir       = invertMatrix.Transform(rayDir);

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
            // ad polygon in collision to resulting list
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
    if (useShader)
    {
        // bind shader program
        m_pColorShader->Use(true);

        // get perspective (or projection) matrix slot from shader
        GLint uniform = QR_OpenGLHelper::GetUniform(m_pColorShader,
                                                    EQR_SA_PerspectiveMatrix);

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
void TMainForm::PrepareShaderToDrawModel(QR_Shader_OpenGL* pShader, const TQRTextures& textures)
{
    if (!pShader)
        return;

    // bind shader program
    pShader->Use(true);

    // get perspective (or projection) matrix slot from shader
    GLint uniform = QR_OpenGLHelper::GetUniform(pShader,
                                                EQR_SA_PerspectiveMatrix);

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
bool TMainForm::LoadTexture(TQRTextures* pTextures, const ITextureTable& textureTable)
{
    if (!pTextures)
        return false;

    // load textures image from resources
    std::auto_ptr<TResourceStream> pTextureBHandStream(new TResourceStream((int)HInstance,
                                                                           ID_MD3_TEXTURE_BHAND,
                                                                           L"DATA"));
    std::auto_ptr<TResourceStream> pTextureClockFaceStream(new TResourceStream((int)HInstance,
                                                                               ID_MD3_TEXTURE_CLOCK_FACE,
                                                                               L"DATA"));
    std::auto_ptr<TResourceStream> pTextureGFClockStream(new TResourceStream((int)HInstance,
                                                                             ID_MD3_TEXTURE_GF_CLOCK,
                                                                             L"DATA"));
    std::auto_ptr<TResourceStream> pTextureGlassPaneStream(new TResourceStream((int)HInstance,
                                                                               ID_MD3_TEXTURE_GLASS_PANE,
                                                                               L"DATA"));
    std::auto_ptr<TResourceStream> pTextureSHandStream(new TResourceStream((int)HInstance,
                                                                           ID_MD3_TEXTURE_SHAND,
                                                                           L"DATA"));

    // iterate through textures to create
    for (std::size_t i = 0; i < textureTable.size(); ++i)
    {
        // reset stream positions
        pTextureBHandStream->Position     = 0;
        pTextureClockFaceStream->Position = 0;
        pTextureGFClockStream->Position   = 0;
        pTextureGlassPaneStream->Position = 0;
        pTextureSHandStream->Position     = 0;

        // create and populate new texture
        std::auto_ptr<TQRTexture> pTexture(new TQRTexture());
        pTexture->Name = textureTable[i].m_Name.c_str();

        // create texture bitmap
        std::auto_ptr<TBitmap> pBitmap(new TBitmap());

        // load MD3 texture
        switch (textureTable[i].m_ID)
        {
            case ID_MD3_TEXTURE_BHAND:      pBitmap->LoadFromStream(pTextureBHandStream.get());     break;
            case ID_MD3_TEXTURE_CLOCK_FACE: pBitmap->LoadFromStream(pTextureClockFaceStream.get()); break;
            case ID_MD3_TEXTURE_GF_CLOCK:   pBitmap->LoadFromStream(pTextureGFClockStream.get());   break;
            case ID_MD3_TEXTURE_GLASS_PANE: pBitmap->LoadFromStream(pTextureGlassPaneStream.get()); break;
            case ID_MD3_TEXTURE_SHAND:      pBitmap->LoadFromStream(pTextureSHandStream.get());     break;
        }

        BYTE* pPixels = NULL;

        try
        {
            // convert bitmap to pixel array, and create OpenGL texture from array
            QR_OpenGLHelper::BytesFromBitmap(pBitmap.get(), pPixels, false, false);
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

        // assign texture to list
        m_Textures.Length += 1;
        m_Textures[i]      = pTexture.get();
        pTexture.release();
    }

    return true;
}
//--------------------------------------------------------------------------------------------------
void TMainForm::DrawModel(TQRMD3Model*         pModel,
                          const TQRTextures    textures,
                          const TQRMatrix4x4&  matrix,
                          NativeInt            index,
                          NativeInt            nextIndex,
                          const double         interpolationFactor,
                          bool                 useShader,
                          bool                 collisions)
{
    // no model to draw?
    if (!pModel)
        return;

    // get mesh count
    const NativeInt meshCount = pModel->GetMeshCount();

    // are indexes out of bounds?
    if (index > meshCount || nextIndex > meshCount)
        return;

    TQRMesh*     pMeshToDraw     = NULL;
    TQRMesh*     pNextMeshToDraw = NULL;
    TQRAABBTree* pAABBTree       = NULL;
    bool         interpolated    = false;

    try
    {
        // get frame to draw, and frame to interpolate with
        IFrame* pFrame     = GetFrame(index,     pModel, collisions);
        IFrame* pNextFrame = GetFrame(nextIndex, pModel, collisions);

        // do use shader?
        if (!useShader)
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
        pAABBTree = pFrame->m_pAABBTree;

        // do use shader?
        if (!useShader)
            // draw mesh
            QR_OpenGLHelper::Draw(*pMeshToDraw, matrix, textures);
        else
        {
            // prepare shader to draw the model
            PrepareShaderToDrawModel(m_pTextureShader, textures);

            // draw mesh
            QR_OpenGLHelper::Draw(*pMeshToDraw,
                                  *pNextMeshToDraw,
                                   matrix,
                                   interpolationFactor,
                                   textures,
                                   m_pTextureShader);
        }
    }
    __finally
    {
        if (interpolated)
            delete pMeshToDraw;
    }

    DetectAndDrawCollisions(matrix, pAABBTree, useShader, collisions);
}
//--------------------------------------------------------------------------------------------------
