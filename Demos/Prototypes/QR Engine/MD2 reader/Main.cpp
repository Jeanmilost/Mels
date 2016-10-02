/******************************************************************************
 * ==> Main ------------------------------------------------------------------*
 ******************************************************************************
 * Description : MD2 demo main form                                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// vcl
#include <Vcl.Graphics.hpp>

// std
#include <sstream>

// qr engine
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
    m_pMD2Animation(NULL),
    m_pTexture(NULL),
    m_PreviousTime(::GetTickCount())
{
    // load resources
    std::auto_ptr<TResourceStream> pModelStream(new TResourceStream((int)HInstance,
                                                                    ID_MD2_MODEL,
                                                                    L"DATA"));
    std::auto_ptr<TResourceStream> pTextureStream(new TResourceStream((int)HInstance,
                                                                      ID_MD2_TEXTURE,
                                                                      L"DATA"));
    std::auto_ptr<TResourceStream> pNTStream(new TResourceStream((int)HInstance,
                                                                 ID_MD2_NORMALS_TABLE,
                                                                 L"DATA"));

    // load MD2 texture
    m_pTexture = new TBitmap();
    m_pTexture->LoadFromStream(pTextureStream.get());

    // load MD2 model
    QR_MemoryBuffer modelBuffer;
    modelBuffer.Write(pModelStream->Memory, pModelStream->Size);
    modelBuffer.Seek(0, 0);
    m_MD2.Load(modelBuffer, modelBuffer.GetSize());

    // load normals table
    QR_MemoryBuffer normalsTableBuffer;
    normalsTableBuffer.Write(pNTStream->Memory, pNTStream->Size);
    normalsTableBuffer.Seek(0, 0);
    m_MD2.LoadNormals(normalsTableBuffer, normalsTableBuffer.GetSize());

    // set vertex format to use
    m_MD2.SetVertexFormat((QR_Vertex::IEFormat)(QR_Vertex::IE_VF_Colors    |
                                                QR_Vertex::IE_VF_TexCoords |
                                                QR_Vertex::IE_VF_Normals));

    // configure precalculated light
    QR_MD2::ILight light;
    light.m_Ambient   = QR_Color(32, 32, 32, 255);
    light.m_Light     = QR_Color(255, 255, 255, 255);
    light.m_Direction = QR_Vector3DP(0.0f, -1.0f, 0.0f);
    m_MD2.SetLight(light);

    // configure color
    m_MD2.SetColor(QR_Color(255, 255, 255, 255));

    // by default, use color mode
    m_MD2.SetLightningMode(QR_MD2::IE_LM_None);

    // build animation generator
    BuildAnimation();
}
//---------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    // delete animation generator
    if (m_pMD2Animation)
        delete m_pMD2Animation;

    // delete used texture
    if (m_pTexture)
        delete m_pTexture;

    // shutdown OpenGL
    QR_WinOpenGLHelper::DisableOpenGL(Handle, m_hDC, m_hRC);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject* pSender)
{
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

    BYTE* pPixels = NULL;

    try
    {
        // convert bitmap to pixel array, and create OpenGL texture from array
        BytesFromBitmap(m_pTexture, pPixels, false, false);
        CreateTexture(m_pTexture->Width,
                      m_pTexture->Height,
                      m_pTexture->PixelFormat == pf32bit ? GL_RGBA : GL_RGB,
                      pPixels,
                      GL_NEAREST,
                      GL_NEAREST,
                      GL_TEXTURE_2D);

        glEnable(GL_TEXTURE_2D);
    }
    __finally
    {
        if (pPixels)
            delete[] pPixels;
    }

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
    gluPerspective(45.0f, aspect, 0.1f, 10000.0f);

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
void __fastcall TMainForm::miLightingClick(TObject* pSender)
{
    // check or uncheck menu item
    miLighting->Checked = !miLighting->Checked;

    // switch between color or precalculated light mode
    m_MD2.SetLightningMode(miLighting->Checked ? QR_MD2::IE_LM_Precalculated : QR_MD2::IE_LM_None);

    // mesh changed, rebuild animation generator
    BuildAnimation();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::miPrevAnimClick(TObject* pSender)
{
    // get current running animation
    const std::string name = m_pMD2Animation->GetRunningAnimationName();

    // select prev animation
    if (name == "Stand")
        m_pMD2Animation->Set("CRDeath4");
    else
    if (name == "Run")
        m_pMD2Animation->Set("Stand");
    else
    if (name == "Attack")
        m_pMD2Animation->Set("Run");
    else
    if (name == "Pain1")
        m_pMD2Animation->Set("Attack");
    else
    if (name == "Pain2")
        m_pMD2Animation->Set("Pain1");
    else
    if (name == "Pain3")
        m_pMD2Animation->Set("Pain2");
    else
    if (name == "Jump")
        m_pMD2Animation->Set("Pain3");
    else
    if (name == "Flip")
        m_pMD2Animation->Set("Jump");
    else
    if (name == "Salute")
        m_pMD2Animation->Set("Flip");
    else
    if (name == "Taunt")
        m_pMD2Animation->Set("Salute");
    else
    if (name == "Wave")
        m_pMD2Animation->Set("Taunt");
    else
    if (name == "Point")
        m_pMD2Animation->Set("Wave");
    else
    if (name == "CRStand")
        m_pMD2Animation->Set("Point");
    else
    if (name == "CRWalk")
        m_pMD2Animation->Set("CRStand");
    else
    if (name == "CRAttack")
        m_pMD2Animation->Set("CRWalk");
    else
    if (name == "CRPain")
        m_pMD2Animation->Set("CRAttack");
    else
    if (name == "CRDeath")
        m_pMD2Animation->Set("CRPain");
    else
    if (name == "CRDeath2")
        m_pMD2Animation->Set("CRDeath");
    else
    if (name == "CRDeath3")
        m_pMD2Animation->Set("CRDeath2");
    else
    if (name == "CRDeath4")
        m_pMD2Animation->Set("CRDeath3");
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::miNextAnimClick(TObject* pSender)
{
    // get current running animation
    const std::string name = m_pMD2Animation->GetRunningAnimationName();

    // select next animation
    if (name == "Stand")
        m_pMD2Animation->Set("Run");
    else
    if (name == "Run")
        m_pMD2Animation->Set("Attack");
    else
    if (name == "Attack")
        m_pMD2Animation->Set("Pain1");
    else
    if (name == "Pain1")
        m_pMD2Animation->Set("Pain2");
    else
    if (name == "Pain2")
        m_pMD2Animation->Set("Pain3");
    else
    if (name == "Pain3")
        m_pMD2Animation->Set("Jump");
    else
    if (name == "Jump")
        m_pMD2Animation->Set("Flip");
    else
    if (name == "Flip")
        m_pMD2Animation->Set("Salute");
    else
    if (name == "Salute")
        m_pMD2Animation->Set("Taunt");
    else
    if (name == "Taunt")
        m_pMD2Animation->Set("Wave");
    else
    if (name == "Wave")
        m_pMD2Animation->Set("Point");
    else
    if (name == "Point")
        m_pMD2Animation->Set("CRStand");
    else
    if (name == "CRStand")
        m_pMD2Animation->Set("CRWalk");
    else
    if (name == "CRWalk")
        m_pMD2Animation->Set("CRAttack");
    else
    if (name == "CRAttack")
        m_pMD2Animation->Set("CRPain");
    else
    if (name == "CRPain")
        m_pMD2Animation->Set("CRDeath");
    else
    if (name == "CRDeath")
        m_pMD2Animation->Set("CRDeath2");
    else
    if (name == "CRDeath2")
        m_pMD2Animation->Set("CRDeath3");
    else
    if (name == "CRDeath3")
        m_pMD2Animation->Set("CRDeath4");
    else
    if (name == "CRDeath4")
        m_pMD2Animation->Set("Stand");
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
    // get mesh vertex format (all the meshes of the model will share the same)
    const QR_Vertex::IEFormat format = m_MD2.GetVertexFormat();

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

    QR_Mesh mesh;

    // get next frame to draw
    m_pMD2Animation->GetMesh(elapsedTime, 10, mesh);

    // iterate through frame meshes
    for (unsigned i = 0; i < mesh.size(); ++i)
    {
        glPushMatrix();
        glTranslatef(0.0f, 0.0f, -100.0f);
        glRotatef(-90, 1.0f, 0.0f, 0.0f);
        glRotatef(-45, 0.0f, 0.0f, 1.0f);

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

        // draw MD2 mesh
        switch (mesh[i]->m_Type)
        {
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
    const std::size_t pixelSize = (pBitmap->PixelFormat == pf32bit) ? sizeof(TRGBQuad) : sizeof(TRGBTriple);

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
int TMainForm::CreateTexture(WORD width, WORD height, WORD format, void* pPixels, GLuint minFilter,
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
//---------------------------------------------------------------------------
void TMainForm::BuildAnimation()
{
    std::string animName;

    // do delete animation generator?
    if (m_pMD2Animation)
    {
        // keep current animation name and delete animation generator
        animName = m_pMD2Animation->GetRunningAnimationName();
        delete m_pMD2Animation;
    }

    m_pMD2Animation = new QR_MD2Animation(&m_MD2, true);
    m_pMD2Animation->Add("Stand",     0,  39);
    m_pMD2Animation->Add("Run",      40,  45);
    m_pMD2Animation->Add("Attack",   46,  53);
    m_pMD2Animation->Add("Pain1",    54,  57);
    m_pMD2Animation->Add("Pain2",    58,  61);
    m_pMD2Animation->Add("Pain3",    62,  65);
    m_pMD2Animation->Add("Jump",     66,  71);
    m_pMD2Animation->Add("Flip",     72,  83);
    m_pMD2Animation->Add("Salute",   84,  94);
    m_pMD2Animation->Add("Taunt",    95,  111);
    m_pMD2Animation->Add("Wave",     112, 122);
    m_pMD2Animation->Add("Point",    123, 134);
    m_pMD2Animation->Add("CRStand",  135, 153);
    m_pMD2Animation->Add("CRWalk",   154, 159);
    m_pMD2Animation->Add("CRAttack", 160, 168);
    m_pMD2Animation->Add("CRPain",   169, 172);
    m_pMD2Animation->Add("CRDeath",  173, 177);
    m_pMD2Animation->Add("CRDeath2", 178, 183);
    m_pMD2Animation->Add("CRDeath3", 184, 189);
    m_pMD2Animation->Add("CRDeath4", 190, 197);

    // set start animation
    if (animName.empty())
        m_pMD2Animation->Set("Stand");
    else
        m_pMD2Animation->Set(animName);
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

