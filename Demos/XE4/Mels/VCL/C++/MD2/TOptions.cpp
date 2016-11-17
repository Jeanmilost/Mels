/**************************************************************************************************
 * ==> Options form ------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Form to allow user to select which and how the MD2 model will be rendered        *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "TOptions.h"

// std
#include <memory>

// engine
#include "QR_GDIHelper.h"
#include "QR_OpenGLHelper.h"

// resources
#include "Resources.rh"

#pragma package(smart_init)
#pragma link "UTQRMD2ModelGroup"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
// TOptions - c++ VCL class
//--------------------------------------------------------------------------------------------------
TOptions* Options;
//--------------------------------------------------------------------------------------------------
__fastcall TOptions::TOptions(TComponent* pOwner) :
    TForm(pOwner),
    m_pMD2(NULL),
    m_ModelRendered(false),
    m_Closing(false)
{
    LoadPreview();
}
//--------------------------------------------------------------------------------------------------
__fastcall TOptions::~TOptions()
{
    // delete model
    if (m_pMD2)
        delete m_pMD2;
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
    gbLoadOptions->Enabled         = !rgCacheOptions->ItemIndex;
    ckShowDefaultFrame->Enabled    =  gbLoadOptions->Enabled;
    ckRunGestureWhenReady->Enabled =  gbLoadOptions->Enabled;
    ckShowCollisions->Enabled      = (rgCacheOptions->ItemIndex != 1);
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
void __fastcall TOptions::tiDrawPreviewTimer(TObject* pSender)
{
    // model already rendered?
    if (m_ModelRendered)
    {
        tiDrawPreview->Enabled = false;

        // delete model
        if (m_pMD2)
        {
            delete m_pMD2;
            m_pMD2 = NULL;
        }

        return;
    }

    // draw model
    m_pMD2->Draw(0.0);
}
//--------------------------------------------------------------------------------------------------
bool TOptions::IsAppClosing() const
{
    return m_Closing;
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
                // really close the application
                Application->Terminate();

            break;
    }

    inherited::WndProc(message);
}
//--------------------------------------------------------------------------------------------------
void TOptions::Reset()
{
    // reset interface to default values
    rgCacheOptions->ItemIndex      = 0;
    ckShowDefaultFrame->Checked    = true;
    ckRunGestureWhenReady->Checked = false;
    ckFullScreen->Checked          = false;
    ckShowCollisions->Checked      = false;
    ckUseShader->Checked           = true;
}
//--------------------------------------------------------------------------------------------------
void TOptions::LoadPreview()
{
    // create MD2 model, populate callbacks
    std::auto_ptr<TQRMD2Group> pMD2(new TQRMD2Group());
    pMD2->OnLoadMeshTexture = OnLoadMeshTexture;
    pMD2->OnCustomDrawItem  = OnDrawCustomModelItem;

    TQRModelOptions       modelOptions;
    TQRFramedModelOptions framedModelOptions;

    // configure model options
    modelOptions << EQR_MO_Without_Normals;

    // load resources
    std::auto_ptr<TResourceStream> pModelStream(new TResourceStream((int)HInstance,
                                                                    ID_MD2_MODEL,
                                                                    L"DATA"));

    // create in-memory model directory
    std::auto_ptr<TQRMemoryDir> pMemDir(new TQRMemoryDir(true));

    if (!pMemDir->AddFile(L"marvin.md2", pModelStream.get(), false))
        return;

    pModelStream.release();

    // place model into 3D world
    *pMD2->Translation =  TQRVector3D(0.0f, 0.0f, -100.0f);
     pMD2->RotationX   = -M_PI_2; // -90°
     pMD2->RotationZ   = -M_PI_4; // -45°

    // set gesture to run
    pMD2->Gesture = 0;

    std::auto_ptr<TQRColor> pColor(new TQRColor(255, 255, 255, 255));

    // load model
    if (!pMD2->Load(pMemDir.get(),
                    L"marvin",
                    pColor.get(),
                    NULL,
                    false,
                    modelOptions,
                    framedModelOptions))
        return;

    pMemDir.release();

    m_pMD2 = pMD2.release();
}
//--------------------------------------------------------------------------------------------------
bool __fastcall TOptions::OnLoadMeshTexture(TQRModelGroup* const pGroup,
                                            TQRModel* const      pModel,
                                            TBitmap*             pBitmap,
                                            TQRTexture*          pTexture,
                                            bool&                loadNext)
{
    // not used here, as the rendering is done once to a bitmap inside the OnDrawCustomModelItem
    // function. As OpenGl is still not initialized when this function is called, the texture also
    // cannot be prepared for now
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

    // get MD2 model
    TQRMD2Model* pMD2Model = static_cast<TQRMD2Model*>(pModel);

    // found it?
    if (!pMD2Model)
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

        TQRMesh mesh;

        // get model first mesh
        pMD2Model->GetMesh(0, mesh, NULL);

        // get texture from stream
        std::auto_ptr<TResourceStream> pTextureStream(new TResourceStream((int)HInstance,
                                                                          ID_MD2_TEXTURE,
                                                                          L"DATA"));

        // load MD2 texture
        std::auto_ptr<TBitmap> pBitmap(new TBitmap());
        pBitmap->LoadFromStream(pTextureStream.get());

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

        // create model texture
        std::auto_ptr<TQRTexture> pLoadedTexture(new TQRTexture());
        pLoadedTexture->Index = textureIndex;
        pLoadedTexture->Name  = mesh[0].m_Name;

        TQRTextures loadedTextures;
        loadedTextures.Length = 1;
        loadedTextures[0]     = pLoadedTexture.get();

        // clear scene
        glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // draw model first mesh
        QR_OpenGLHelper::Draw(mesh, matrix, loadedTextures);

        glFlush();

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
