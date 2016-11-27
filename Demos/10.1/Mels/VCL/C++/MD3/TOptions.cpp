/**************************************************************************************************
 * ==> Options form ------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Form to allow user to select which and how the MD3 model will be rendered        *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "TOptions.h"

// std
#include <memory>

// qr engine
#include "QR_GDIHelper.h"
#include "QR_OpenGLHelper.h"

// resources
#include "Resources.rh"

#pragma package(smart_init)
#pragma link "UTQRMD3ModelGroup"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
// TOptions - c++ VCL class
//--------------------------------------------------------------------------------------------------
TOptions* Options;
//--------------------------------------------------------------------------------------------------
__fastcall TOptions::TOptions(TComponent* pOwner) :
    TForm(pOwner),
    m_pMD3(NULL),
    m_Team(EQR_PT_MD3_Default),
    m_ModelRendered(false),
    m_Closing(false)
{
    // load resources
    std::auto_ptr<TResourceStream> pModelStream(new TResourceStream((int)HInstance,
                                                                    ID_MD3_MODEL,
                                                                    PWideChar(L"DATA")));

    // load preview
    LoadPreview(pModelStream.release());
}
//--------------------------------------------------------------------------------------------------
__fastcall TOptions::~TOptions()
{
    // delete textures
    for (ITextures::iterator it = m_Textures.begin(); it != m_Textures.end(); ++it)
        delete (*it);

    // delete model
    if (m_pMD3)
        delete m_pMD3;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::FormCreate(TObject* pSender)
{
    // to see form in taskbar even if main form is still not created
    ::SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::rgCacheOptionsClick(TObject* pSender)
{
    // enable advanced cache options only if "create cache" option is selected
    ckShowCollisions->Enabled = (rgCacheOptions->ItemIndex != 1);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::btQuitClick(TObject* pSender)
{
    m_Closing = true;
    Application->Terminate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::btCancelClick(TObject* pSender)
{
    Reset();
    Close();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::btOKClick(TObject* pSender)
{
    Close();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::btBrowseClick(TObject* pSender)
{
    // previous MD3 model was already loaded?
    if (m_pMD3)
    {
        // query job status
        TQRModelJobStatus* pStatus = m_pMD3->QueryJobStatus();

        // was previous model fully loaded and drawn?
        if (pStatus && pStatus->Status != EQR_JS_Done && pStatus->Status != EQR_JS_Error)
            return;
    }

    // clear previous interface
    edModelFileName->Text    = L"";
    odOpenDialog->InitialDir = ::ExtractFilePath(Application->ExeName);

    // show open file dialog to user and check if dialog was canceled
    if (!odOpenDialog->Execute())
        return;

    // file exists?
    if (!::FileExists(odOpenDialog->FileName))
        return;

    // show selected file name
    edModelFileName->Text = odOpenDialog->FileName;

    // open package stream
    std::auto_ptr<TFileStream> pFileStream(new TFileStream(edModelFileName->Text, fmOpenRead));

    // load model preview
    if (!LoadPreview(pFileStream.release()))
    {
        m_Team                = EQR_PT_MD3_Default;
        rbDefault->Checked    = true;
        edModelFileName->Text = L"";
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::tiDrawPreviewTimer(TObject* pSender)
{
    // model already rendered?
    if (m_ModelRendered)
    {
        tiDrawPreview->Enabled = false;

        // delete model
        if (m_pMD3)
        {
            delete m_pMD3;
            m_pMD3 = NULL;
        }

        return;
    }

    TQRModelJobStatus* pStatus = m_pMD3->QueryJobStatus();

    if (pStatus && pStatus->Status == EQR_JS_Error)
    {
        m_ModelRendered        = true;
        tiDrawPreview->Enabled = false;

        ::MessageDlg("Failed to open model.", mtError, TMsgDlgButtons() << mbOK, 0);

        // delete model
        if (m_pMD3)
        {
            delete m_pMD3;
            m_pMD3 = NULL;
        }

        return;
    }

    if (!pStatus || pStatus->Status != EQR_JS_Done)
        return;

    // calculate rendering surface width and height (4x higher to allow 4x4 antialiasing to be
    // applied later)
    const int width  = imPreview->Width  * 4;
    const int height = imPreview->Height * 4;

    // create overlay render surface (cannot use a bitmap directly, unfortunately)
    std::auto_ptr<TForm> pOverlayForm(new TForm((TComponent*)NULL));
    pOverlayForm->ClientWidth  = width;
    pOverlayForm->ClientHeight = height;
    pOverlayForm->Visible      = false;

    HDC   hDC;
    HGLRC hRC;

    // initialize OpenGL
    if (!QR_OpenGLHelper::EnableOpenGL(pOverlayForm->Handle, hDC, hRC))
        return;

    try
    {
        // configure OpenGL
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_FRONT);
        glEnable(GL_TEXTURE_2D);

        // create viewport
        QR_OpenGLHelper::CreateViewport(width, height, true);

        // clear scene
        glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // draw model
        m_pMD3->Draw(0.0);

        // create image overlay
        std::auto_ptr<TBitmap> pOverlay(new TBitmap());
        QR_OpenGLHelper::GetBitmapFromOpenGL(pOverlay.get());

        // create antialiased final image overlay
        std::auto_ptr<TBitmap> pAntialiasedOverlay(new TBitmap());

        // apply 4x4 antialiasing on the rendered image
        QR_GDIHelper::ApplyAntialiasing(pOverlay.get(), pAntialiasedOverlay.get(), 4);

        // show final image
        imPreview->Picture->Assign(pAntialiasedOverlay.get());
    }
    __finally
    {
        // shutdown OpenGL
        QR_OpenGLHelper::DisableOpenGL(pOverlayForm->Handle, hDC, hRC);
    }

    m_ModelRendered = true;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::OnSelectTeam(TObject* pSender)
{
    EQRMD3PackageTeam selectedTeam = EQR_PT_MD3_Default;

    // get newly selected team
    if (rbDefault->Checked)
        selectedTeam = EQR_PT_MD3_Default;
    else
    if (rbRed->Checked)
        selectedTeam = EQR_PT_MD3_Red;
    else
    if (rbBlue->Checked)
        selectedTeam = EQR_PT_MD3_Blue;

    // nothing to do?
    if (m_Team == selectedTeam)
        return;

    // change team
    EQRMD3PackageTeam prevTeam = m_Team;
    m_Team                     = selectedTeam;

    std::auto_ptr<TStream> pStream;

    // open package stream
    if (edModelFileName->Text.IsEmpty())
        pStream.reset(new TResourceStream((int)HInstance, ID_MD3_MODEL, PWideChar(L"DATA")));
    else
        pStream.reset(new TFileStream(edModelFileName->Text, fmOpenRead));

    // load model preview
    if (!LoadPreview(pStream.release()))
    {
        // restore previous team
        m_Team = prevTeam;

        // restore previous selection
        switch (m_Team)
        {
            case EQR_PT_MD3_Default: rbDefault->Checked = true; break;
            case EQR_PT_MD3_Red:     rbRed->Checked     = true; break;
            case EQR_PT_MD3_Blue:    rbBlue->Checked    = true; break;
            default:                 throw "Unknown team";
        }
    }
}
//--------------------------------------------------------------------------------------------------
bool TOptions::IsAppClosing() const
{
    return m_Closing;
}
//--------------------------------------------------------------------------------------------------
EQRMD3PackageTeam TOptions::GetSelectedTeam() const
{
    return m_Team;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::WndProc(TMessage& message)
{
    // dispatch message
    switch (message.Msg)
    {
        case WM_SYSCOMMAND:
            // close button was clicked on form?
            if (message.WParam == SC_CLOSE)
            {
                m_Closing = true;

                // really close the application
                Application->Terminate();
            }

            break;
    }

    #ifdef __llvm__
        // 64 bit compiler cannot access the base function using the inherited TForm directive,
        // because it was declared as private under several RAD Studio versions
        TForm::WndProc(message);
    #else
        inherited::WndProc(message);
    #endif
}
//--------------------------------------------------------------------------------------------------
void TOptions::Reset()
{
    // reset interface to default values
    rgCacheOptions->ItemIndex = 0;
    ckFullScreen->Checked     = false;
    ckShowCollisions->Checked = false;
    ckUseShader->Checked      = true;
}
//--------------------------------------------------------------------------------------------------
bool TOptions::LoadPreview(TStream* pStream)
{
    std::auto_ptr<TStream> pModelStream(pStream);

    // clear previous interface
    tiDrawPreview->Enabled = false;
    imPreview->Picture->Assign(NULL);

    // delete previous textures
    for (ITextures::iterator it = m_Textures.begin(); it != m_Textures.end(); ++it)
        delete (*it);

    m_Textures.clear();

    // delete previous model
    if (m_pMD3)
    {
        delete m_pMD3;
        m_pMD3 = NULL;
    }

    // no stream to load?
    if (!pModelStream.get())
        return false;

    // create MD3 model, populate callbacks
    std::auto_ptr<TQRMD3Group> pMD3(new TQRMD3Group());
    pMD3->OnLoadMeshTexture = OnLoadMeshTexture;
    pMD3->OnCustomDrawItem  = OnDrawCustomModelItem;

    TQRModelOptions       modelOptions;
    TQRFramedModelOptions framedModelOptions;

    // configure model options
    modelOptions << EQR_MO_Without_Normals;

    std::auto_ptr<TQRColor> pColor(new TQRColor(255, 255, 255, 255));

    if (!pMD3->Load(pModelStream.release(),
                    pColor.get(),
                    false,
                    modelOptions,
                    framedModelOptions,
                    m_Team))
        return false;

    // set default animation gesture
    pMD3->SetAnimation(L"upper", EQR_AG_MD3_Torso_Stand);
    pMD3->SetAnimation(L"lower", EQR_AG_MD3_Legs_Walk);

    // place model into 3D world
    *pMD3->Translation = TQRVector3D(0.0f, -5.0f, -100.0f);
     pMD3->RotationX   = -M_PI_2; // -90°
     pMD3->RotationY   = -M_PI_4; // -45°

    m_pMD3 = pMD3.release();

    m_ModelRendered        = false;
    tiDrawPreview->Enabled = true;

    return true;
}
//--------------------------------------------------------------------------------------------------
bool __fastcall TOptions::OnLoadMeshTexture(TQRModelGroup* const pGroup,
                                            TQRModel* const      pModel,
                                            TBitmap*             pBitmap,
                                            TQRTexture*          pTexture,
                                            bool&                loadNext)
{
    // OpenGL is still not initialized when textures are loaded, this is why it's not possible to
    // link them here. To workaround that, textures are stored inside a set of bitmaps, and their
    // addresses are used as a key to retrieve them later, when required
    if (pBitmap)
    {
        std::auto_ptr<TBitmap> pModelTexture(new TBitmap());
        pModelTexture->Assign(pBitmap);
        m_Textures.insert(pModelTexture.get());
        pTexture->CustomData = pModelTexture.get();
        pModelTexture.release();
    }
    else
        pTexture->Enabled = false;

    return true;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::OnDrawCustomModelItem(TQRModelGroup* const pGroup,
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

    TQRMesh mesh;

    // get mesh to draw
    pMD3Model->GetMesh(index, mesh, NULL);

    // no mesh to draw?
    if (!mesh.Length)
        return;

    const std::size_t textureCount = textures.Length;
          TQRTextures modelTextures;

    try
    {
        modelTextures.Length = textureCount;

        // iterate through textures to link
        for (std::size_t i = 0; i < textureCount; ++i)
        {
            modelTextures[i] = NULL;

            // is texture enabled?
            if (!textures[i]->Enabled)
                continue;

            // get previoulsy loaded texture to link in set
            ITextures::iterator it = m_Textures.find((TBitmap*)textures[i]->CustomData);

            // found it?
            if (it != m_Textures.end())
            {
                std::auto_ptr<TBitmap> pBitmap(new TBitmap());

                // make sure texture is a power of 2 texture (OpenGL may not support non POT textures)
                TQRModelGroupHelper::MakeTexturePowerOf2(*it, pBitmap.get());

                BYTE* pPixels = NULL;
                GLint textureIndex;

                try
                {
                    // convert texture bitmap to pixel array, and create OpenGL texture from array
                    QR_OpenGLHelper::BytesFromBitmap(pBitmap.get(), pPixels, false, false);
                    textureIndex = QR_OpenGLHelper::CreateTexture(pBitmap->Width,
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

                // set textures to use
                modelTextures[i]        = new TQRTexture();
                modelTextures[i]->Name  = textures[i]->Name;
                modelTextures[i]->Index = textureIndex;
            }
        }

        // draw mesh
        QR_OpenGLHelper::Draw(mesh, matrix, modelTextures);
    }
    __finally
    {
        // clear memory
        for (std::size_t i = 0; i < textureCount; ++i)
            if (modelTextures[i])
                delete modelTextures[i];
    }

    glFlush();
}
//--------------------------------------------------------------------------------------------------
