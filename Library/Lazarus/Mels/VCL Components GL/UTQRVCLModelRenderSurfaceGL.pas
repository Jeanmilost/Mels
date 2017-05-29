// *************************************************************************************************
// * ==> UTQRVCLModelRenderSurfaceGL --------------------------------------------------------------*
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
 @abstract(@name provides a renderer surface on which an OpenGL scene can be drawn.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRVCLModelRenderSurfaceGL;

{$MODE Delphi}

interface

uses Graphics,
     Controls,
     Forms,
     Gl,
     GLext,
     Windows,
     UTQRHelpers,
     UTQRVCLHelpers,
     UTQRVCLHelpersGL,
     UTQRVCLModelRendererGL,
     UTQRLogging;

type
    {$REGION 'Documentation'}
    {**
     Renderer surface on which an OpenGL scene can be drawn
     @exclude(todo -cImprovement -oJean: Unfortunately the performances of this class are low, among
                                         others because of number of required copies to provide the
                                         final scene, especially while the antialiasing system is
                                         enabled. With OpenGL it is possible to do better, however
                                         my knowledge of OpenGL and Lazarus is limited. But in the
                                         future, with more experience, this system should be improved,
                                         or replaced by a better solution.)
    }
    {$ENDREGION}
    TQRVCLModelRenderSurfaceGL = class(TObject)
        private
            m_pOwner:      TWinControl;
            m_pRenderer:   TQRVCLModelRendererGL;
            m_pOverlay:    TForm;
            m_hGLContext:  THandle;
            m_Width:       NativeInt;
            m_Height:      NativeInt;
            m_Factor:      NativeInt;
            m_Transparent: Boolean;
            m_Allowed:     Boolean;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner Renderer surface owner)
             @param(pRenderer OpenGL renderer to use)
            }
            {$ENDREGION}
            constructor Create(pOwner: TWinControl; pRenderer: TQRVCLModelRendererGL); virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Initializes the renderer surface
             @param(factor Scale factor to use for antialiasing)
             @param(transparent If @true, alpha transparency will be enabled)
             @param(supportGDI If @true, renderer surface will support embedded GDI drawing)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Be careful, embedded GDI and OpenGL drawing may cause flickering on
                             intensive rendering
            }
            {$ENDREGION}
            function Initialize(factor: NativeInt;
               transparent, supportGDI: Boolean): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Releases the renderer surface
            }
            {$ENDREGION}
            procedure Release; virtual;

            {$REGION 'Documentation'}
            {**
             Resizes the renderer surface
            }
            {$ENDREGION}
            procedure Resize; virtual;

            {$REGION 'Documentation'}
            {**
             Enables OpenGL context (i.e. next OpenGL operation will be done on this surface)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function EnableContext: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Begins to draw a scene
             @return(@true if scene can be drawn, otherwise @false)
            }
            {$ENDREGION}
            function BeginScene: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Ends to draw a scene
            }
            {$ENDREGION}
            procedure EndScene; virtual;

            {$REGION 'Documentation'}
            {**
             Gets scene as pixel array
             @param(pixels @bold([out]) Pixel array to populate)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function GetPixels(var pixels: TQRByteArray): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Gets scene as bitmap
             @param(pBitmap Destination bitmap)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function GetBitmap(pBitmap: Graphics.TBitmap): Boolean; virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the OpenGL context
            }
            {$ENDREGION}
            property GLContext: THandle read m_hGLContext;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLModelComponent
//--------------------------------------------------------------------------------------------------
constructor TQRVCLModelRenderSurfaceGL.Create(pOwner: TWinControl; pRenderer: TQRVCLModelRendererGL);
begin
    inherited Create;

    // create local variables
    m_pOwner      := pOwner;
    m_pRenderer   := pRenderer;
    m_pOverlay    := nil;
    m_hGLContext  := 0;
    m_Width       := 0;
    m_Height      := 0;
    m_Factor      := 1;
    m_Transparent := False;
    m_Allowed     := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLModelRenderSurfaceGL.Destroy;
begin
    m_pOverlay.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.Initialize(factor: NativeInt;
                              transparent, supportGDI: Boolean): Boolean;
var
    hOverlayDC: THandle;
begin
    // no owner?
    if (not Assigned(m_pOwner)) then
        Exit(False);

    // no renderer?
    if (not Assigned(m_pRenderer)) then
        Exit(False);

    // release previous instance if exists
    Release;

    m_Factor      := factor;
    m_Transparent := transparent;

    // update surface size
    m_Width  := m_pOwner.ClientWidth;
    m_Height := m_pOwner.ClientHeight;

    // create overlay render surface (cannot use a bitmap directly, unfortunately)
    m_pOverlay              := TForm.Create(nil);
    m_pOverlay.BorderStyle  := bsNone;
    m_pOverlay.BorderIcons  := [];
    m_pOverlay.ClientWidth  := Screen.DesktopWidth  - 1; // required, otherwise a strange clip may happen on resize (Lazarus bug?)
    m_pOverlay.ClientHeight := Screen.DesktopHeight - 1; // required, otherwise a strange clip may happen on resize (Lazarus bug?)
    m_pOverlay.Visible      := False;
    m_pOverlay.HandleNeeded;

    hOverlayDC := GetDC(m_pOverlay.Handle);

    // device context should exists
    if (hOverlayDC = 0) then
        Exit(False);

    try
        // start OpenGL instance
        if (not m_pRenderer.EnableOpenGL(True, hOverlayDC, m_hGLContext)) then
        begin
            TQRLogHelper.LogToCompiler('TQRVCLModelRenderSurfaceGL - FAILED - Could not create render context');
            Release;
            Exit(False);
        end;

        // check if OpenGL Extension is already initialized. If not, try to initialize it
        if (not TQRVCLOpenGLHelper.InitializeOpenGL) then
        begin
            TQRLogHelper.LogToCompiler('TQRVCLModelRenderSurfaceGL - FAILED - Could not initialize the OpenGL Extension module');
            Release;
            Exit(False);
        end;
    finally
        ReleaseDC(m_pOverlay.Handle, hOverlayDC);
    end;

    m_Allowed := True;
    Result    := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelRenderSurfaceGL.Release;
begin
    // no owner?
    if (not Assigned(m_pOwner)) then
        Exit;

    // no renderer?
    if (not Assigned(m_pRenderer)) then
        Exit;

    // OpenGL was initialized correctly and surface is allowed to work?
    if (m_Allowed and (m_hGLContext <> 0)) then
        // shutdown OpenGL instance
        m_pRenderer.DisableOpenGL(0, 0, m_hGLContext);

    m_pOverlay.Free;
    m_pOverlay := nil;

    // reset values
    m_hGLContext  := 0;
    m_Width       := 0;
    m_Height      := 0;
    m_Factor      := 1;
    m_Transparent := False;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelRenderSurfaceGL.Resize;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
        Exit;

    // no owner?
    if (not Assigned(m_pOwner)) then
        Exit;

    // owner control size changed?
    if ((m_Width = m_pOwner.ClientWidth) and (m_Height = m_pOwner.ClientHeight)) then
        Exit;

    // enable render surface context
    if (not EnableContext) then
        Exit;

    m_Width  := m_pOwner.ClientWidth;
    m_Height := m_pOwner.ClientHeight;

    // update overlay size
    if (Assigned(m_pOverlay)) then
    begin
        m_pOverlay.ClientWidth  := m_Width  * m_Factor;
        m_pOverlay.ClientHeight := m_Height * m_Factor;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.EnableContext: Boolean;
var
    hOverlayDC: THandle;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
        Exit(False);

    // OpenGL should be enabled
    if (m_hGLContext = 0) then
        Exit(False);

    hOverlayDC := GetDC(m_pOverlay.Handle);

    // device context should exists
    if (hOverlayDC = 0) then
        Exit(False);

    try
        // make render context as OpenGL current context
        Result := wglMakeCurrent(hOverlayDC, m_hGLContext);
    finally
        ReleaseDC(m_pOverlay.Handle, hOverlayDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.BeginScene: Boolean;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
        Exit(False);

    // make render context as OpenGL current context
    if (not EnableContext) then
        Exit(False);

    // configure OpenGL depth testing
    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_TRUE);
    glDepthFunc(GL_LEQUAL);
    glDepthRange(0.0, 1.0);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelRenderSurfaceGL.EndScene;
var
    hOverlayDC: THandle;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
        Exit;

    // OpenGL should be enabled
    if (m_hGLContext = 0) then
        Exit;

    hOverlayDC := GetDC(m_pOverlay.Handle);

    // device context should exists
    if (hOverlayDC = 0) then
        Exit;

    try
        SwapBuffers(hOverlayDC);
    finally
        ReleaseDC(m_pOverlay.Handle, hOverlayDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.GetPixels(var pixels: TQRByteArray): Boolean;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
        Exit(False);

    // no owner?
    if (not Assigned(m_pOwner)) then
        Exit(False);

    // size is empty or does not match with owner?
    if ((m_Width  <= 0)                    or
        (m_Height <= 0)                    or
        (m_Width  <> m_pOwner.ClientWidth) or
        (m_Height <> m_pOwner.ClientHeight))
    then
        Exit(False);

    // make render context as OpenGL current context
    if (not EnableContext) then
        Exit(False);

    glFinish;

    glPixelStorei(GL_PACK_ALIGNMENT,   4);
    glPixelStorei(GL_PACK_ROW_LENGTH,  0);
    glPixelStorei(GL_PACK_SKIP_ROWS,   0);
    glPixelStorei(GL_PACK_SKIP_PIXELS, 0);

    // create pixels buffer
    SetLength(pixels, (m_pOwner.ClientWidth * m_Factor) * (m_pOwner.ClientHeight * m_Factor) * 4);

    // copy scene from OpenGL to pixels buffer
    glReadPixels(0,
                 0,
                 m_pOwner.ClientWidth  * m_Factor,
                 m_pOwner.ClientHeight * m_Factor,
                 GL_RGBA,
                 GL_UNSIGNED_BYTE,
                 pixels);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.GetBitmap(pBitmap: Graphics.TBitmap): Boolean;
var
    pixels:                  TQRByteArray;
    pLine24:                 PQRRGBTripleArray;
    pLine32:                 PQRRGBQuadArray;
    x, y:                    NativeInt;
    offset:                  NativeUInt;
    alphaValue, alphaFactor: Single;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
        Exit(False);

    glFlush;

    glReadBuffer(GL_FRONT);

    try
        // get OpenGL scene as pixel array
        if (not GetPixels(pixels)) then
            Exit(False);

        offset := 0;

        // search for pixel format to apply
        case (pBitmap.PixelFormat) of
            pf24bit:
            begin
                // iterate through image lines
                for y := 0 to pBitmap.Height - 1 do
                begin
                    // get next line to copy
                    pLine24 := PQRRGBTripleArray(pBitmap.ScanLine[(pBitmap.Height - y) - 1]);

                    // iterate through line pixels
                    for x := 0 to pBitmap.Width - 1 do
                    begin
                        // copy pixel to destination bitmap
                        pLine24[x].rgbtRed   := pixels[offset];
                        pLine24[x].rgbtGreen := pixels[offset + 1];
                        pLine24[x].rgbtBlue  := pixels[offset + 2];

                        Inc(offset, 4);
                    end;
                end;
            end;

            pf32bit:
            begin
                // iterate through image lines
                for y := 0 to pBitmap.Height - 1 do
                begin
                    // get next line to copy
                    pLine32 := PQRRGBQuadArray(pBitmap.ScanLine[(pBitmap.Height - y) - 1]);

                    // iterate through line pixels
                    for x := 0 to pBitmap.Width - 1 do
                    begin
                        alphaValue := pixels[offset + 3];

                        // calculate alpha factor to use to premultiply color components
                        alphaFactor := alphaValue / 255.0;

                        // copy pixel to destination bitmap
                        pLine32[x].rgbRed      := Trunc(alphaFactor * pixels[offset]);
                        pLine32[x].rgbGreen    := Trunc(alphaFactor * pixels[offset + 1]);
                        pLine32[x].rgbBlue     := Trunc(alphaFactor * pixels[offset + 2]);
                        pLine32[x].rgbReserved := Trunc(alphaFactor * pixels[offset + 3]);

                        Inc(offset, 4);
                    end;
                end;
            end;
        else
            Exit(False);
        end;
    finally
        // clear memory
        SetLength(pixels, 0);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------

end.
