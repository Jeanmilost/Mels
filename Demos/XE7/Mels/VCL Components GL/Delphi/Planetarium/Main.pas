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

{**
 @abstract(@name contains the planetarium demo main form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit Main;

interface

uses System.Classes,
     System.SysUtils,
     System.Variants,
     Vcl.Graphics,
     Vcl.Imaging.jpeg,
     Vcl.Imaging.pngimage,
     Vcl.Controls,
     Vcl.ExtCtrls,
     Vcl.StdCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Winapi.Messages,
     Winapi.Windows,
     Winapi.OpenGL,
     UTQR3D,
     UTQRGeometry,
     UTQRHelpers,
     UTQRVCLHelpers,
     UTQRVCLModelComponentGL,
     UTQRVCLShapeComponentGL,
     UTQRVCLModelRendererGL,
     UTQRVCLModelShaderGL;

type
    {**
     Main form
    }
    TMainForm = class(TForm)
        published
            sbMain: TScrollBox;
            paMercury: TPanel;
            spMercury: TQRVCLSphereGL;
            tiAnimation: TTimer;
            laMercury: TLabel;
            paMercuryMain: TPanel;
            laMercuryDesc: TLabel;
            paVenus: TPanel;
            spVenus: TQRVCLSphereGL;
            paVenusMain: TPanel;
            laVenus: TLabel;
            laVenusDesc: TLabel;
            paEarth: TPanel;
            spEarth: TQRVCLSphereGL;
            paEarthMain: TPanel;
            laEarth: TLabel;
            laEarthDesc: TLabel;
            paMars: TPanel;
            spMars: TQRVCLSphereGL;
            paMarsMain: TPanel;
            laMain: TLabel;
            laMarsDesc: TLabel;
            paMoon: TPanel;
            spMoon: TQRVCLSphereGL;
            paMoonMain: TPanel;
            laMoon: TLabel;
            laMoonDesc: TLabel;
            paJupiter: TPanel;
            spJupiter: TQRVCLSphereGL;
            paJupiterMain: TPanel;
            laJupiter: TLabel;
            laJupiterDesc: TLabel;
            paSaturn: TPanel;
            spSaturn: TQRVCLSphereGL;
            paSaturnMain: TPanel;
            laSaturn: TLabel;
            laSaturnDesc: TLabel;
            paUranus: TPanel;
            spUranus: TQRVCLSphereGL;
            paUranusMain: TPanel;
            laUranus: TLabel;
            laUranusDesc: TLabel;
            paNeptune: TPanel;
            spNeptune: TQRVCLSphereGL;
            paNeptuneMain: TPanel;
            laNeptune: TLabel;
            laNeptuneDesc: TLabel;

            function spSaturnCreateSceneMatrix(pSender: TObject;
                                  var projectionMatrix,
                                            viewMatrix: TQRMatrix4x4;
                                            hDC, hGLRC: NativeUInt;
                                             pRenderer: TQRVCLModelRendererGL;
                                               pShader: TQRVCLModelShaderGL): Boolean;
            procedure spSaturnLoadTexture(pSender: TObject;
                                       hDC, hGLRC: NativeUInt;
                                        pRenderer: TQRVCLModelRendererGL;
                                          pShader: TQRVCLModelShaderGL);
            procedure spSaturnAfterDrawScene(pSender: TObject;
                                          hDC, hGLRC: NativeUInt;
                                           pRenderer: TQRVCLModelRendererGL;
                                             pShader: TQRVCLModelShaderGL);
            procedure tiAnimationTimer(pSender: TObject);

        private
            m_Rings:        TQRMesh;
            m_RingTextures: TQRTextures;
            m_Angle:        Single;

            {**
             Generates a ring
             @param(slices Number of slices composing the ring)
             @param(innerRadius Inner radius)
             @param(outerRadius Outer radius)
             @param(mesh @bold([out]) Generated ring mesh)
            }
            procedure GenerateRing(slices: NativeUInt;
                 innerRadius, outerRadius: Single;
                                 out mesh: TQRMesh);

        public
            {**
             Constructor
             @param (pOwner Form owner)
            }
            constructor Create(pOwner: TComponent); override;

            {**
             Destructor
            }
            destructor Destroy(); override;
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_Angle := 0.0;

    SetLength(m_RingTextures, 1);
    m_RingTextures[0] := TQRTexture.Create;

    // generate a ring for the Saturn planet
    GenerateRing(20, 0.8, 1.6, m_Rings);
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.Destroy;
begin
    inherited Destroy;

    // delete the Saturn planet ring
    m_RingTextures[0].Free;
    SetLength(m_Rings, 0);
end;
//--------------------------------------------------------------------------------------------------
function TMainForm.spSaturnCreateSceneMatrix(pSender: TObject;
                                var projectionMatrix,
                                          viewMatrix: TQRMatrix4x4;
                                          hDC, hGLRC: NativeUInt;
                                           pRenderer: TQRVCLModelRendererGL;
                                             pShader: TQRVCLModelShaderGL): Boolean;
var
    position, direction, up: TQRVector3D;
begin
    // create projection matrix (will not be modified while execution)
    projectionMatrix := pRenderer.GetOrtho(-1.0, 1.0, -1.0, 1.0, -100.0, 100.0);

    position  := TQRVector3D.Create(0.0, 0.0, 0.0);
    direction := TQRVector3D.Create(0.0, 0.0, 1.0);
    up        := TQRVector3D.Create(0.0, 1.0, 0.0);

    // create view matrix (will not be modified while execution)
    viewMatrix := pRenderer.LookAtLH(position, direction, up);

    // load projection matrix and initialize it
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;

    // apply projection matrix
    glLoadMatrix(PGLfloat(projectionMatrix.GetPtr));

    // load model view matrix and initialize it
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;

    // apply model view matrix
    glLoadMatrix(PGLfloat(viewMatrix.GetPtr));

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.spSaturnLoadTexture(pSender: TObject;
                                     hDC, hGLRC: NativeUInt;
                                      pRenderer: TQRVCLModelRendererGL;
                                        pShader: TQRVCLModelShaderGL);
var
    hPackageInstance: THandle;
    pPNG:             TPngImage;
    pBitmap:          Vcl.Graphics.TBitmap;
    pStream:          TResourceStream;
    pixelFormat:      GLenum;
    pixels:           TQRByteArray;
begin
    pPNG    := nil;
    pBitmap := nil;
    pStream := nil;

    try
        pPNG := TPngImage.Create;

        try
            // get module instance at which this control belongs
            hPackageInstance := FindClassHInstance(TMainForm);

            // found module and package containing the texture JPEG image to load?
            if ((hPackageInstance <> 0) and
                (FindResource(hPackageInstance, PChar('ID_SATURN_RING_TEXTURE'), RT_RCDATA) <> 0))
            then
            begin
                // load JPEG texture image from stream
                pStream := TResourceStream.Create(hPackageInstance,
                                                  PChar('ID_SATURN_RING_TEXTURE'),
                                                  RT_RCDATA);
                pPNG.LoadFromStream(pStream);
            end;
        finally
            // delete resource stream, if needed
            pStream.Free
        end;

        // failed to load JPEG image?
        if ((pPNG.Width = 0) or (pPNG.Height = 0)) then
            Exit;

        // create and configure destination bitmap
        pBitmap             := Vcl.Graphics.TBitmap.Create;
        pBitmap.PixelFormat := pf32bit;
        pBitmap.AlphaFormat := afPremultiplied;
        pBitmap.Width       := pPNG.Width;
        pBitmap.Height      := pPNG.Height;
        pBitmap.Canvas.Draw(0, 0, pPNG);

        // select pixel format to use
        if (pBitmap.PixelFormat = pf32bit) then
            pixelFormat := GL_RGBA
        else
            pixelFormat := GL_RGB;

        try
            // convert bitmap to pixel array, and create OpenGL texture from array
            TQRVCLPictureHelper.BytesFromBitmap(pBitmap, pixels, false, false);
            m_RingTextures[0].Index := pRenderer.CreateTexture(pBitmap.Width,
                                                               pBitmap.Height,
                                                               pixelFormat,
                                                               pixels,
                                                               GL_NEAREST,
                                                               GL_NEAREST,
                                                               GL_TEXTURE_2D);
        finally
            SetLength(pixels, 0);
        end;
    finally
        pBitmap.Free;
        pPNG.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.spSaturnAfterDrawScene(pSender: TObject;
                                        hDC, hGLRC: NativeUInt;
                                         pRenderer: TQRVCLModelRendererGL;
                                           pShader: TQRVCLModelShaderGL);
begin
    // enable alpha blending and configure a semi transparent effect on the ring
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_SRC_COLOR);

    // draw the ring
    pRenderer.Draw(m_Rings,
                   TQRVector3D.Create(0.0, 0.0, -0.5),
                   4.5,
                   0.16,
                   m_Angle,
                   TQRVector3D.Create(1.0, 1.0, 1.0),
                   m_RingTextures);

    // disable alpha blending
    glDisable(GL_BLEND);

    glFlush;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.tiAnimationTimer(pSender: TObject);
begin
    // calculate next angle
    m_Angle := m_Angle + 0.025;

    // limit angle to 2 * PI value
    if (m_Angle > PI * 2) then
        m_Angle := m_Angle - (PI * 2);

    // update mercury planet
    spMercury.Model.RotationY := m_Angle;
    spMercury.Invalidate;

    // update venus planet
    spVenus.Model.RotationY := m_Angle;
    spVenus.Invalidate;

    // update earth planet
    spEarth.Model.RotationY := m_Angle;
    spEarth.Invalidate;

    // update moon
    spMoon.Model.RotationY := m_Angle;
    spMoon.Invalidate;

    // update mars planet
    spMars.Model.RotationY := m_Angle;
    spMars.Invalidate;

    // update jupiter planet
    spJupiter.Model.RotationY := m_Angle;
    spJupiter.Invalidate;

    // update saturn planet
    spSaturn.Model.RotationY := m_Angle;
    spSaturn.Invalidate;

    // update uranus planet
    spUranus.Model.RotationY := m_Angle;
    spUranus.Invalidate;

    // update neptune planet
    spNeptune.Model.RotationY := m_Angle;
    spNeptune.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.GenerateRing(slices: NativeUInt;
               innerRadius, outerRadius: Single;
                               out mesh: TQRMesh);
var
    i, offset: NativeUInt;
begin
    // create and populate a vertex buffer for the ring
    SetLength(mesh, 1);
    mesh[0].m_Type      := EQR_VT_TriangleStrip;
    mesh[0].m_CoordType := EQR_VC_XYZ;
    mesh[0].m_Stride    := 5;
    Include(mesh[0].m_Format, EQR_VF_TexCoords);
    SetLength(mesh[0].m_Buffer, (slices + 1) * (mesh[0].m_Stride * 2));

    offset := 0;

    // calculate the ring vertex positions and texture coordinates
    for i := 0 to slices do
    begin
        mesh[0].m_Buffer[offset]     := outerRadius * Cos((i * 2.0 * PI) / slices);
        mesh[0].m_Buffer[offset + 1] := outerRadius * Sin((i * 2.0 * PI) / slices);
        mesh[0].m_Buffer[offset + 2] := 0.0;
        mesh[0].m_Buffer[offset + 3] := i * (1.0 / slices);
        mesh[0].m_Buffer[offset + 4] := 1.0;

        mesh[0].m_Buffer[offset + 5] := innerRadius * Cos((i * 2.0 * PI) / slices);
        mesh[0].m_Buffer[offset + 6] := innerRadius * Sin((i * 2.0 * PI) / slices);
        mesh[0].m_Buffer[offset + 7] := 0.0;
        mesh[0].m_Buffer[offset + 8] := i * (1.0 / slices);
        mesh[0].m_Buffer[offset + 9] := 0.0;

        // go to next polygon
        Inc(offset, mesh[0].m_Stride * 2);
    end;
end;
//--------------------------------------------------------------------------------------------------

end.
