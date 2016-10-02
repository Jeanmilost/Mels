/******************************************************************************
 * ==> Main ------------------------------------------------------------------*
 ******************************************************************************
 * Description : MD3 demo main form                                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/


#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// vcl
#include <Vcl.Graphics.hpp>

// std
#include <math.h>
#include <sstream>

// qr engine
#include "QR_FileTools.h"
#include "QR_MemoryBuffer.h"
#include "QR_WinOpenGLHelper.h"

// resources
#include "Resources.rh"

#pragma package(smart_init)
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
// Global defines
//---------------------------------------------------------------------------
#define GL_CLAMP_TO_EDGE 0x812F
//---------------------------------------------------------------------------
// TMainForm
//---------------------------------------------------------------------------
TMainForm* MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_hDC(NULL),
    m_hRC(NULL),
    m_pMemoryDir(NULL),
    m_PreviousTime(::GetTickCount()),
    m_CurTorsoGesture(QR_MD3AnimCfgFile::IE_AG_TORSO_STAND),
    m_CurLegsGesture(QR_MD3AnimCfgFile::IE_AG_LEGS_WALK)
{}
//---------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    // delete memory dir
    if (m_pMemoryDir)
        delete m_pMemoryDir;

    // shutdown OpenGL
    QR_WinOpenGLHelper::DisableOpenGL(Handle, m_hDC, m_hRC);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject* pSender)
{
    // load resources and create memory dir
    if (!CreateAndPopulateMemoryDir())
    {
        MessageDlg("Failed to load model from resources.\r\n\r\nApplication will close.", mtError,
                TMsgDlgButtons() << mbOK, 0);;
        Application->Terminate();
        return;
    }

    // initialize OpenGL
    if (!QR_WinOpenGLHelper::EnableOpenGL(Handle, m_hDC, m_hRC))
    {
        MessageDlg("OpenGL could not be initialized.\r\n\r\nApplication will close.", mtError,
                TMsgDlgButtons() << mbOK, 0);;
        Application->Terminate();
        return;
    }

    // configure OpenGL
    ConfigOpenGL();

    glEnable(GL_TEXTURE_2D);

    m_MD3Model.Set_OnLoadTexture(OnLoadTexture);
    m_MD3Model.Set_OnDrawItem(OnDrawItem);

    QR_MD3Group::IInfo info;
    info.m_ModelTemplate = L"elf_%s";
    info.m_SkinTemplate  = L"elf_%s";
    info.m_AnimTemplate  = L"elf_%s";

    // load model group from dir
    if (!m_MD3Model.Load(*m_pMemoryDir, info))
    {
        MessageDlg("Failed to open model.\r\n\r\nApplication will close.", mtError,
                TMsgDlgButtons() << mbOK, 0);;
        Application->Terminate();
        return;
    }

    // set default animation gesture
    m_MD3Model.SetAnimation(L"elf_upper", QR_MD3AnimCfgFile::IE_AG_TORSO_STAND);
    m_MD3Model.SetAnimation(L"elf_lower", QR_MD3AnimCfgFile::IE_AG_LEGS_WALK);

    // locate model in space
    m_MD3Model.SetTranslation(QR_Vector3DP(0.0f, -10.0f, -120.0f));
    m_MD3Model.SetRotationX(-(M_PI / 2.0f));
    m_MD3Model.SetRotationY(M_PI + (M_PI / 4.0f));

    // from now, OpenGL will draw scene every time the thread do nothing else
    Application->OnIdle = IdleLoop;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormResize(TObject* pSender)
{
    // get client size
    int w = ClientWidth;
    int h = ClientHeight;

    // invalid width?
    if (!w)
        w = 1;

    // invalid height?
    if (!h)
        h = 1;

    // set viewport
    glViewport(0, 0, w, h);

    // load projection matrix and initialize it
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    // apply perspective correction
    GLfloat aspect = (GLfloat)w/(GLfloat)h;
    gluPerspective(45.0f, aspect, 0.1f, 100000.0f);

    // load model view matrix and initialize it
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormPaint(TObject* pSender)
{
    RenderGLScene();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::miPrevTorsoAnimClick(TObject* pSender)
{
    --m_CurTorsoGesture;

    if (m_CurTorsoGesture < QR_MD3AnimCfgFile::IE_AG_BOTH_DEATH1)
        m_CurTorsoGesture = QR_MD3AnimCfgFile::IE_AG_TORSO_STAND2;

    // set new torso animation gesture
    m_MD3Model.SetAnimation(L"elf_upper", (QR_MD3AnimCfgFile::IEGesture)m_CurTorsoGesture);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::miNextTorsoAnimClick(TObject* pSender)
{
    ++m_CurTorsoGesture;

    if (m_CurTorsoGesture > QR_MD3AnimCfgFile::IE_AG_TORSO_STAND2)
        m_CurTorsoGesture = QR_MD3AnimCfgFile::IE_AG_BOTH_DEATH1;

    // set new torso animation gesture
    m_MD3Model.SetAnimation(L"elf_upper", (QR_MD3AnimCfgFile::IEGesture)m_CurTorsoGesture);
}
//------------------------------------------------------------------------------
void __fastcall TMainForm::miPrevLegsAnimClick(TObject* pSender)
{
    --m_CurLegsGesture;

    if (m_CurLegsGesture < QR_MD3AnimCfgFile::IE_AG_BOTH_DEATH1)
        m_CurLegsGesture = QR_MD3AnimCfgFile::IE_AG_LEGS_TURN;

    if (m_CurLegsGesture > QR_MD3AnimCfgFile::IE_AG_BOTH_DEAD3 &&
        m_CurLegsGesture < QR_MD3AnimCfgFile::IE_AG_LEGS_WALK_CROUCHING)
        m_CurLegsGesture = QR_MD3AnimCfgFile::IE_AG_BOTH_DEAD3;

    // set new legs animation gesture
    m_MD3Model.SetAnimation(L"elf_lower", (QR_MD3AnimCfgFile::IEGesture)m_CurLegsGesture);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::miNextLegsAnimClick(TObject* pSender)
{
    ++m_CurLegsGesture;

    if (m_CurLegsGesture > QR_MD3AnimCfgFile::IE_AG_LEGS_TURN)
        m_CurLegsGesture = QR_MD3AnimCfgFile::IE_AG_BOTH_DEATH1;

    if (m_CurLegsGesture > QR_MD3AnimCfgFile::IE_AG_BOTH_DEAD3 &&
        m_CurLegsGesture < QR_MD3AnimCfgFile::IE_AG_LEGS_WALK_CROUCHING)
        m_CurLegsGesture = QR_MD3AnimCfgFile::IE_AG_LEGS_WALK_CROUCHING;

    // set new legs animation gesture
    m_MD3Model.SetAnimation(L"elf_lower", (QR_MD3AnimCfgFile::IEGesture)m_CurLegsGesture);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::IdleLoop(TObject* pSender, bool& done)
{
    done = false;
    RenderGLScene();
}
//---------------------------------------------------------------------------
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

    SwapBuffers(m_hDC);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Draw(const double& elapsedTime)
{
    m_MD3Model.Draw(elapsedTime);
}
//------------------------------------------------------------------------------
bool TMainForm::OnLoadTexture(QR_ModelGroup::ITexture* pTexture)
{
    // no texture?
    if (!pTexture)
        return false;

    // get stream containing texture to load
    QR_MemoryBuffer* pBuffer = m_pMemoryDir->GetFile(pTexture->m_Name);
    std::auto_ptr<TMemoryStream> pStream(new TMemoryStream());
    pStream->Write(pBuffer->GetPtr(), pBuffer->GetSize());
    pStream->Seek((__int64)0, soBeginning);

    // load texture
    std::auto_ptr<TPicture> pPicture(new TPicture());
    pPicture->Bitmap->LoadFromStream(pStream.get());

    // convert to bitmap
    std::auto_ptr<TBitmap> pBitmap(new TBitmap());
    pBitmap->Assign(pPicture.get());

    BYTE* pPixels = NULL;

    try
    {
        // convert bitmap to pixel array, and create OpenGL texture from array
        BytesFromBitmap(pBitmap.get(), pPixels, false, false);
        pTexture->m_Index = CreateTexture(pBitmap->Width,
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
//------------------------------------------------------------------------------
void TMainForm::OnDrawItem(const QR_Model*                 pModel,
                           const QR_ModelGroup::ITextures& textures,
                           const QR_Matrix16P&             matrix,
                           const QR_Mesh*                  pMesh,
                           const QR_Mesh*                  pNextMesh,
                                 QR_Double                 interpolationFactor)
{
    if (!pModel)
        return;

    if (!pMesh)
        return;

    if (!pNextMesh)
        return;

    QR_Mesh mesh;

    // process interpolation and send resulting mesh to output
    if (QR_MD3::Interpolate(interpolationFactor, *pMesh, *pNextMesh, mesh) != QR_MD3Common::IE_C_Success)
        return;

    if (textures.size() < 1)
        M_THROW_EXCEPTION("Incorrect texture count");

    glBindTexture(GL_TEXTURE_2D, textures[0]->m_Index);

    // get mesh vertex format (all the meshes of the model will share the same)
    const QR_Vertex::IEFormat format = pModel->GetVertexFormat();

    // calculate vertex size
    std::size_t vertexSize = 3;

    // do include normals?
    if (format & QR_Vertex::IE_VF_Normals)
        vertexSize += 3;

    // do include texture coordinates?
    if (format & QR_Vertex::IE_VF_TexCoords)
        vertexSize += 2;

    // do include vertex color?
    if (format & QR_Vertex::IE_VF_Colors)
        vertexSize += 4;

    // iterate through frame meshes
    for (unsigned i = 0; i < mesh.size(); ++i)
    {
        glPushMatrix();
        glLoadMatrixf(matrix.GetPtr());

        // bind vertex array
        glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(3, GL_FLOAT, vertexSize * sizeof(M_Precision), &mesh[i]->m_Buffer[0]);

        std::size_t offset = 3;

        // bind normals array
        if (format & QR_Vertex::IE_VF_Normals)
        {
            glEnableClientState(GL_NORMAL_ARRAY);
            glNormalPointer(GL_FLOAT,
                            vertexSize * sizeof(M_Precision),
                            &mesh[i]->m_Buffer[offset]);

            offset += 3;
        }

        // bind texture coordinates array
        if (format & QR_Vertex::IE_VF_TexCoords)
        {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glTexCoordPointer(2,
                              GL_FLOAT,
                              vertexSize * sizeof(M_Precision),
                              &mesh[i]->m_Buffer[offset]);

            offset += 2;
        }

        // bind colors array
        if (format & QR_Vertex::IE_VF_Colors)
        {
            glEnableClientState(GL_COLOR_ARRAY);
            glColorPointer(4,
                           GL_FLOAT,
                           vertexSize * sizeof(M_Precision),
                           &mesh[i]->m_Buffer[offset]);
        }

        // draw MD3 mesh
        switch (mesh[i]->m_Type)
        {
            case QR_Vertex::IE_VT_Triangles:
                glDrawArrays(GL_TRIANGLES, 0, mesh[i]->m_Buffer.size() / vertexSize);
                break;

            case QR_Vertex::IE_VT_TriangleStrip:
                glDrawArrays(GL_TRIANGLE_STRIP, 0, mesh[i]->m_Buffer.size() / vertexSize);
                break;

            case QR_Vertex::IE_VT_TriangleFan:
                glDrawArrays(GL_TRIANGLE_FAN, 0, mesh[i]->m_Buffer.size() / vertexSize);
                break;
        }

        // unbind vertex array
        glDisableClientState(GL_VERTEX_ARRAY);

        // unbind normals array
        if (format & QR_Vertex::IE_VF_Normals)
            glDisableClientState(GL_NORMAL_ARRAY);

        // unbind texture coordinates array
        if (format & QR_Vertex::IE_VF_TexCoords)
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);

        // unbind colors array
        if (format & QR_Vertex::IE_VF_Colors)
            glDisableClientState(GL_COLOR_ARRAY);

        glFlush();
        glPopMatrix();

        // mesh is now useless, delete it
        delete mesh[i];
    }
}
//------------------------------------------------------------------------------
bool TMainForm::CreateAndPopulateMemoryDir()
{
    // create memory dir
    m_pMemoryDir = new QR_ModelGroup::IMemoryDir(true);

    // load files from resources
    std::auto_ptr<TResourceStream> pElfBmp(new TResourceStream((int)HInstance,
                                                               ID_ELF_BMP,
                                                               L"DATA"));
    std::auto_ptr<TResourceStream> pElfAnimCfg(new TResourceStream((int)HInstance,
                                                                   ID_ELF_ANMATION_CFG,
                                                                   L"DATA"));
    std::auto_ptr<TResourceStream> pElfHeadMD3(new TResourceStream((int)HInstance,
                                                                   ID_ELF_HEAD_MD3,
                                                                   L"DATA"));
    std::auto_ptr<TResourceStream> pElfHeadSkin(new TResourceStream((int)HInstance,
                                                                    ID_ELF_HEAD_SKIN,
                                                                    L"DATA"));
    std::auto_ptr<TResourceStream> pElfLowerMD3(new TResourceStream((int)HInstance,
                                                                    ID_ELF_LOWER_MD3,
                                                                    L"DATA"));
    std::auto_ptr<TResourceStream> pElfLowerSkin(new TResourceStream((int)HInstance,
                                                                     ID_ELF_LOWER_SKIN,
                                                                     L"DATA"));
    std::auto_ptr<TResourceStream> pElfUpperMD3(new TResourceStream((int)HInstance,
                                                                    ID_ELF_UPPER_MD3,
                                                                    L"DATA"));
    std::auto_ptr<TResourceStream> pElfUpperSkin(new TResourceStream((int)HInstance,
                                                                     ID_ELF_UPPER_SKIN,
                                                                     L"DATA"));

    // create elf bitmap texture memory file
    std::auto_ptr<QR_MemoryBuffer> pElfBmpBuffer(new QR_MemoryBuffer());
    pElfBmpBuffer->Write(pElfBmp->Memory, pElfBmp->Size);
    pElfBmpBuffer->Seek(0, 0);

    // add file to memory dir
    if (!m_pMemoryDir->AddFile(L"elf.bmp", pElfBmpBuffer.get(), false))
        return false;

    pElfBmpBuffer.release();

    // create elf animation config memory file
    std::auto_ptr<QR_MemoryBuffer> pElfAnimCfgBuffer(new QR_MemoryBuffer());
    pElfAnimCfgBuffer->Write(pElfAnimCfg->Memory, pElfAnimCfg->Size);
    pElfAnimCfgBuffer->Seek(0, 0);

    // add file to memory dir
    if (!m_pMemoryDir->AddFile(L"elf_animation.cfg", pElfAnimCfgBuffer.get(), false))
        return false;

    pElfAnimCfgBuffer.release();

    // create elf head MD3 sub-model memory file
    std::auto_ptr<QR_MemoryBuffer> pElfHeadMD3Buffer(new QR_MemoryBuffer());
    pElfHeadMD3Buffer->Write(pElfHeadMD3->Memory, pElfHeadMD3->Size);
    pElfHeadMD3Buffer->Seek(0, 0);

    // add file to memory dir
    if (!m_pMemoryDir->AddFile(L"elf_head.md3", pElfHeadMD3Buffer.get(), false))
        return false;

    pElfHeadMD3Buffer.release();

    // create elf head skin memory file
    std::auto_ptr<QR_MemoryBuffer> pElfHeadSkinBuffer(new QR_MemoryBuffer());
    pElfHeadSkinBuffer->Write(pElfHeadSkin->Memory, pElfHeadSkin->Size);
    pElfHeadSkinBuffer->Seek(0, 0);

    // add file to memory dir
    if (!m_pMemoryDir->AddFile(L"elf_head.skin", pElfHeadSkinBuffer.get(), false))
        return false;

    pElfHeadSkinBuffer.release();

    // create elf lower MD3 sub-model memory file
    std::auto_ptr<QR_MemoryBuffer> pElfLowerMD3Buffer(new QR_MemoryBuffer());
    pElfLowerMD3Buffer->Write(pElfLowerMD3->Memory, pElfLowerMD3->Size);
    pElfLowerMD3Buffer->Seek(0, 0);

    // add file to memory dir
    if (!m_pMemoryDir->AddFile(L"elf_lower.md3", pElfLowerMD3Buffer.get(), false))
        return false;

    pElfLowerMD3Buffer.release();

    // create elf lower skin memory file
    std::auto_ptr<QR_MemoryBuffer> pElfLowerSkinBuffer(new QR_MemoryBuffer());
    pElfLowerSkinBuffer->Write(pElfLowerSkin->Memory, pElfLowerSkin->Size);
    pElfLowerSkinBuffer->Seek(0, 0);

    // add file to memory dir
    if (!m_pMemoryDir->AddFile(L"elf_lower.skin", pElfLowerSkinBuffer.get(), false))
        return false;

    pElfLowerSkinBuffer.release();

    // create elf upper MD3 sub-model memory file
    std::auto_ptr<QR_MemoryBuffer> pElfUpperMD3Buffer(new QR_MemoryBuffer());
    pElfUpperMD3Buffer->Write(pElfUpperMD3->Memory, pElfUpperMD3->Size);
    pElfUpperMD3Buffer->Seek(0, 0);

    // add file to memory dir
    if (!m_pMemoryDir->AddFile(L"elf_upper.md3", pElfUpperMD3Buffer.get(), false))
        return false;

    pElfUpperMD3Buffer.release();

    // create elf upper skin memory file
    std::auto_ptr<QR_MemoryBuffer> pElfUpperSkinBuffer(new QR_MemoryBuffer());
    pElfUpperSkinBuffer->Write(pElfUpperSkin->Memory, pElfUpperSkin->Size);
    pElfUpperSkinBuffer->Seek(0, 0);

    // add file to memory dir
    if (!m_pMemoryDir->AddFile(L"elf_upper.skin", pElfUpperSkinBuffer.get(), false))
        return false;

    pElfUpperSkinBuffer.release();

    return true;
}
//------------------------------------------------------------------------------
bool TMainForm::BytesFromBitmap(TBitmap* pBitmap, BYTE*& pPixels, bool flipY, bool bgr)
{
    // no bitmap?
    if (!pBitmap)
        return false;

    // is bitmap empty?
    if ((pBitmap->Width <= 0) || (pBitmap->Height <= 0))
        return false;

    // get bitmap and pixel size
    const std::size_t width     = pBitmap->Width;
    const std::size_t height    = pBitmap->Height;
    const std::size_t pixelSize =
            (pBitmap->PixelFormat == pf32bit) ? sizeof(TRGBQuad) : sizeof(TRGBTriple);

    // calculate line size
    const std::size_t lineSize = width * pixelSize;

    // create pixels buffer
    pPixels = new BYTE[height * lineSize];

    // iterate through bitmap lines
    for (std::size_t y = 0; y < height; ++y)
    {
        // calculate next offset
        const std::size_t offset = flipY ? ((height - 1) - y) * lineSize : y * lineSize;

        // is 24 or 32 bit bitmap?
        if (pBitmap->PixelFormat == pf24bit)
        {
            // get pixels line from bitmap
            TRGBTriple* pLineRGB = reinterpret_cast<TRGBTriple*>(pBitmap->ScanLine[y]);

            // do swap pixels?
            if (bgr)
                // memory copy 24 bit pixels line, as pixels are already in RGB format
                std::memcpy(&pPixels[offset], pLineRGB, lineSize);
            else
                // iterate through line pixels
                for (std::size_t x = 0; x < width; ++x)
                {
                    // calculate next pixel offset
                    const std::size_t offsetX = offset + (x * pixelSize);

                    // copy and swap pixel
                    pPixels[offsetX]     = pLineRGB[x].rgbtRed;
                    pPixels[offsetX + 1] = pLineRGB[x].rgbtGreen;
                    pPixels[offsetX + 2] = pLineRGB[x].rgbtBlue;
                }
        }
        else
        {
            // get pixels line from bitmap
            TRGBQuad* pLineRGBA = reinterpret_cast<TRGBQuad*>(pBitmap->ScanLine[y]);

            // do swap pixels?
            if (bgr)
                // memory copy 32 bit pixels line, as pixels are already in RGB format
                std::memcpy(&pPixels[offset], pLineRGBA, lineSize);
            else
                // iterate through line pixels
                for (std::size_t x = 0; x < width; ++x)
                {
                    // calculate next pixel offset
                    const std::size_t offsetX = offset + (x * pixelSize);

                    // copy and swap pixel
                    pPixels[offsetX]     = pLineRGBA[x].rgbRed;
                    pPixels[offsetX + 1] = pLineRGBA[x].rgbGreen;
                    pPixels[offsetX + 2] = pLineRGBA[x].rgbBlue;
                    pPixels[offsetX + 3] = pLineRGBA[x].rgbReserved;
                }
        }
    }

    return true;
}
//------------------------------------------------------------------------------
GLuint TMainForm::CreateTexture(WORD width, WORD height, WORD format, void* pPixels, GLuint minFilter,
        GLuint magFilter, GLuint targetID)
{
    GLuint texture;

    // create and bind new OpenGL texture
    glGenTextures(1, &texture);
    glBindTexture(targetID, texture);

    // set texture environment parameters
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    // set texture filtering
    glTexParameteri(targetID, GL_TEXTURE_MIN_FILTER, minFilter);
    glTexParameteri(targetID, GL_TEXTURE_MAG_FILTER, magFilter);

    // set texture wrapping mode
    glTexParameteri(targetID, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(targetID, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    // generate texture from bitmap data
    glTexImage2D(targetID, 0, format, width, height, 0, format, GL_UNSIGNED_BYTE, pPixels);

    return texture;
}
//------------------------------------------------------------------------------
void TMainForm::ConfigOpenGL()
{
    // configure OpenGL
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);
}
//---------------------------------------------------------------------------
