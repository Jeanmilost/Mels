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
#include <memory>

// opengl
#include <gl\gl.h>

// resources
#include "Resources.rh"

#pragma package(smart_init)
#pragma link "UTQRVCLModelComponentGL"
#pragma link "UTQRVCLShapeComponentGL"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_Angle(0.0f)
{
    m_RingTextures.Length = 1;
    m_RingTextures[0]     = new TQRTexture;

    // generate a ring for the Saturn planet
    GenerateRing(20, 0.8, 1.6, m_Rings);
}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    // delete the Saturn planet ring
    delete m_RingTextures[0];
    m_Rings.Length = 0;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::spSaturnAfterDrawScene(TObject*               pSender,
                                                  NativeUInt             hDC,
                                                  NativeUInt             hGLRC,
                                                  TQRVCLModelRendererGL* pRenderer,
                                                  TQRVCLModelShaderGL*   pShader)
{
    // enable alpha blending and configure a semi transparent effect on the ring
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_SRC_COLOR);

    // draw the ring
    pRenderer->Draw(m_Rings,
                    TQRVector3D(0.0, 0.0, -0.5),
                    4.5,
                    0.16,
                    m_Angle,
                    TQRVector3D(1.0, 1.0, 1.0),
                    m_RingTextures);

    // disable alpha blending
    glDisable(GL_BLEND);

    glFlush();
}
//--------------------------------------------------------------------------------------------------
bool __fastcall TMainForm::spSaturnCreateSceneMatrix(TObject*               pSender,
                                                     TQRMatrix4x4&          projectionMatrix,
                                                     TQRMatrix4x4&          viewMatrix,
                                                     NativeUInt             hDC,
                                                     NativeUInt             hGLRC,
                                                     TQRVCLModelRendererGL* pRenderer,
                                                     TQRVCLModelShaderGL*   pShader)
{
    // create projection matrix (will not be modified while execution)
    projectionMatrix = pRenderer->GetOrtho(-1.0, 1.0, -1.0, 1.0, -100.0, 100.0);

    TQRVector3D position(0.0, 0.0, 0.0);
    TQRVector3D direction(0.0, 0.0, 1.0);
    TQRVector3D up(0.0, 1.0, 0.0);

    // create view matrix (will not be modified while execution)
    viewMatrix = pRenderer->LookAtLH(position, direction, up);

    // load projection matrix and initialize it
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    // apply projection matrix
    glLoadMatrixf(PGLfloat(projectionMatrix.GetPtr()));

    // load model view matrix and initialize it
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    // apply model view matrix
    glLoadMatrixf(PGLfloat(viewMatrix.GetPtr()));

    return true;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::spSaturnLoadTexture(TObject*               pSender,
                                               NativeUInt             hDC,
                                               NativeUInt             hGLRC,
                                               TQRVCLModelRendererGL* pRenderer,
                                               TQRVCLModelShaderGL*   pShader)
{
    // load sound from resources
    std::auto_ptr<TResourceStream> pStream(new TResourceStream((int)HInstance,
                                                               ID_SATURN_RING_TEXTURE,
                                                               PWideChar(L"DATA")));

    std::auto_ptr<TPngImage> pPNG(new TPngImage());
    pPNG->LoadFromStream(pStream.get());

    // failed to load JPEG image?
    if (!pPNG->Width || !pPNG->Height)
        return;

    // create and configure destination bitmap
    std::auto_ptr<TBitmap> pBitmap(new TBitmap());
    pBitmap->PixelFormat = pf32bit;
    pBitmap->AlphaFormat = afPremultiplied;
    pBitmap->Width       = pPNG->Width;
    pBitmap->Height      = pPNG->Height;
    pBitmap->Canvas->Draw(0, 0, pPNG.get());

    GLenum pixelFormat;

    // select pixel format to use
    if (pBitmap->PixelFormat == pf32bit)
        pixelFormat = GL_RGBA;
    else
        pixelFormat = GL_RGB;

    TQRByteArray pixels;

    try
    {
        // convert bitmap to pixel array, and create OpenGL texture from array
        TQRVCLPictureHelper::BytesFromBitmap(pBitmap.get(), pixels, false, false);
        m_RingTextures[0]->Index = pRenderer->CreateTexture(pBitmap->Width,
                                                            pBitmap->Height,
                                                            pixelFormat,
                                                            &pixels[0],
                                                            GL_NEAREST,
                                                            GL_NEAREST,
                                                            GL_TEXTURE_2D);
    }
    __finally
    {
        pixels.Length = 0;
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::tiAnimationTimer(TObject* pSender)
{
    // calculate next angle
    m_Angle += 0.025f;

    // limit angle to 2 * PI value
    if (m_Angle > M_PI * 2)
        m_Angle -= (M_PI * 2);

    // update mercury planet
    spMercury->Model->RotationY = m_Angle;
    spMercury->Invalidate();

    // update venus planet
    spVenus->Model->RotationY = m_Angle;
    spVenus->Invalidate();

    // update earth planet
    spEarth->Model->RotationY = m_Angle;
    spEarth->Invalidate();

    // update moon
    spMoon->Model->RotationY = m_Angle;
    spMoon->Invalidate();

    // update mars planet
    spMars->Model->RotationY = m_Angle;
    spMars->Invalidate();

    // update jupiter planet
    spJupiter->Model->RotationY = m_Angle;
    spJupiter->Invalidate();

    // update saturn planet
    spSaturn->Model->RotationY = m_Angle;
    spSaturn->Invalidate();

    // update uranus planet
    spUranus->Model->RotationY = m_Angle;
    spUranus->Invalidate();

    // update neptune planet
    spNeptune->Model->RotationY = m_Angle;
    spNeptune->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void TMainForm::GenerateRing(std::size_t slices,
                             float       innerRadius,
                             float       outerRadius,
                             TQRMesh&    mesh)
{
    // create and populate a vertex buffer for the ring
    mesh.Length = 1;
    mesh[0].m_Format << EQR_VF_TexCoords;
    mesh[0].m_Type          = EQR_VT_TriangleStrip;
    mesh[0].m_CoordType     = EQR_VC_XYZ;
    mesh[0].m_Stride        = 5;
    mesh[0].m_Buffer.Length = (slices + 1) * (mesh[0].m_Stride * 2);

    std::size_t offset = 0;

    // calculate the ring vertex positions and texture coordinates
    for (std::size_t i = 0; i <= slices; ++i)
    {
        mesh[0].m_Buffer[offset]     = outerRadius * std::cos((i * 2.0f * M_PI) / slices);
        mesh[0].m_Buffer[offset + 1] = outerRadius * std::sin((i * 2.0f * M_PI) / slices);
        mesh[0].m_Buffer[offset + 2] = 0.0f;
        mesh[0].m_Buffer[offset + 3] = i * (1.0f / slices);
        mesh[0].m_Buffer[offset + 4] = 1.0f;

        mesh[0].m_Buffer[offset + 5] = innerRadius * std::cos((i * 2.0f * M_PI) / slices);
        mesh[0].m_Buffer[offset + 6] = innerRadius * std::sin((i * 2.0f * M_PI) / slices);
        mesh[0].m_Buffer[offset + 7] = 0.0f;
        mesh[0].m_Buffer[offset + 8] = i * (1.0f / slices);
        mesh[0].m_Buffer[offset + 9] = 0.0f;

        // go to next polygon
        offset += (mesh[0].m_Stride * 2);
    }
}
//--------------------------------------------------------------------------------------------------
