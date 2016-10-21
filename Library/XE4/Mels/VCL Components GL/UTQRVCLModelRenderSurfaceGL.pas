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
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRVCLModelRenderSurfaceGL;

interface
    // do not include XE7.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE XE7.OpenGLext *)

uses UTQRCommon,
     UTQRHelpers,
     UTQRVCLHelpers,
     UTQRVCLHelpersGL,
     UTQRVCLModelRendererGL,
     UTQRLogging,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.Forms,
     Winapi.Windows,
     // unfortunately the required OpenGL headers does not exist or are incomplete in XE4 and
     // earlier, so the DelphiGL component (provided with installation) should be used instead
     XE7.OpenGL,
     XE7.OpenGLext;

type
    {$REGION 'Documentation'}
    {**
     Renderer surface on which an OpenGL scene can be drawn
    }
    {$ENDREGION}
    TQRVCLModelRenderSurfaceGL = class(TObject)
        private
            m_Allowed:             Boolean;
            m_pOwner:              TWinControl;
            m_pRenderer:           TQRVCLModelRendererGL;
            m_hGLContext:          THandle;
            m_OverlayFrameBuffer:  GLuint;
            m_OverlayRenderBuffer: GLuint;
            m_OverlayDepthBuffer:  GLuint;
            m_Width:               NativeInt;
            m_Height:              NativeInt;
            m_Factor:              NativeInt;
            m_Transparent:         Boolean;

        protected
            {$REGION 'Documentation'}
            {**
             Creates buffer to use for alpha rendering
             @param(hDC Device context that OpenGL will use to render to)
             @param(width Surface width, in pixels)
             @param(height Surface height, in pixels)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function CreateARGBRenderBuffers(hDC: THandle; width, height: NativeInt): Boolean;

            {$REGION 'Documentation'}
            {**
             Clears buffer used for alpha rendering
             @param(hDC Device context used by OpenGL to render to)
            }
            {$ENDREGION}
            procedure ClearARGBRenderBuffers(hDC: THandle);

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
             @param(hDC Device context that OpenGL will use to render to)
             @param(factor Scale factor to use for antialiasing)
             @param(transparent If @true, alpha transparency will be enabled)
             @param(supportGDI If @true, renderer surface will support embedded GDI drawing)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Be careful, embedded GDI and OpenGL drawing may cause flickering on
                             intensive rendering
            }
            {$ENDREGION}
            function Initialize(hDC: THandle;
                             factor: NativeInt;
            transparent, supportGDI: Boolean): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Releases the renderer surface
             @param(hDC Device context used by OpenGL to render to)
            }
            {$ENDREGION}
            procedure Release(hDC: THandle); virtual;

            {$REGION 'Documentation'}
            {**
             Resizes the renderer surface
             @param(hDC Device context that OpenGL will use to render to)
            }
            {$ENDREGION}
            procedure Resize(hDC: THandle); virtual;

            {$REGION 'Documentation'}
            {**
             Enables OpenGL context (i.e. next OpenGL operation will be done on this surface)
             @param(hDC Device context that OpenGL will use to render to)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function EnableContext(hDC: THandle): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Begins to draw a scene
             @param(hDC Device context that OpenGL will use to render to)
             @return(@true if scene can be drawn, otherwise @false)
            }
            {$ENDREGION}
            function BeginScene(hDC: THandle): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Ends to draw a scene
             @param(hDC Device context that OpenGL will use to render to)
            }
            {$ENDREGION}
            procedure EndScene(hDC: THandle); virtual;

            {$REGION 'Documentation'}
            {**
             Gets scene as pixel array
             @param(hDC Device context used by OpenGL to render the scene)
             @param(pixels @bold([out]) Pixel array to populate)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function GetPixels(hDC: THandle; var pixels: TQRByteArray): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Gets scene as bitmap
             @param(hDC Device context used by OpenGL to render the scene)
             @param(pBitmap Destination bitmap)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function GetBitmap(hDC: THandle; pBitmap: Vcl.Graphics.TBitmap): Boolean; virtual;

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
    m_pOwner              := pOwner;
    m_pRenderer           := pRenderer;
    m_hGLContext          := 0;
    m_OverlayRenderBuffer := 0;
    m_OverlayDepthBuffer  := 0;
    m_OverlayFrameBuffer  := 0;
    m_Width               := 0;
    m_Height              := 0;
    m_Factor              := 1;
    m_Transparent         := False;
    m_Allowed             := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLModelRenderSurfaceGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.CreateARGBRenderBuffers(hDC: THandle;
                                                  width, height: NativeInt): Boolean;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    // make render context as OpenGL current context
    if (not EnableContext(hDC)) then
    begin
        Result := False;
        Exit;
    end;

    if (width <= 0) then
        width := 1;

    if (height <= 0) then
        height := 1;

    ClearARGBRenderBuffers(hDC);

    // create main render buffer
    glGenFramebuffers(1, @m_OverlayFrameBuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, m_OverlayFrameBuffer);

    // create and link color buffer to render to
    glGenRenderbuffers(1, @m_OverlayRenderBuffer);
    glBindRenderbuffer(GL_RENDERBUFFER, m_OverlayRenderBuffer);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_RGBA8, width, height);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER,
                              GL_COLOR_ATTACHMENT0,
                              GL_RENDERBUFFER,
                              m_OverlayRenderBuffer);

    // create and link depth buffer to use
    glGenRenderbuffers(1, @m_OverlayDepthBuffer);
    glBindRenderbuffer(GL_RENDERBUFFER, m_OverlayDepthBuffer);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, width, height);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER,
                              GL_DEPTH_ATTACHMENT,
                              GL_RENDERBUFFER,
                              m_OverlayDepthBuffer);

    // check if render buffers were created correctly and return result
    Result := (glCheckFramebufferStatus(GL_FRAMEBUFFER) = GL_FRAMEBUFFER_COMPLETE);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelRenderSurfaceGL.ClearARGBRenderBuffers(hDC: THandle);
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
        Exit;

    // make render context as OpenGL current context
    if (not EnableContext(hDC)) then
        Exit;

    // unbind draw buffer
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

    // delete depth buffer
    if (m_OverlayDepthBuffer <> 0) then
        glDeleteRenderbuffers(1, @m_OverlayDepthBuffer);

    // delete render buffer
    if (m_OverlayRenderBuffer <> 0) then
        glDeleteRenderbuffers(1, @m_OverlayRenderBuffer);

    // delete frame buffer
    if (m_OverlayFrameBuffer <> 0) then
        glDeleteFramebuffers(1, @m_OverlayFrameBuffer);

    m_OverlayDepthBuffer  := 0;
    m_OverlayRenderBuffer := 0;
    m_OverlayFrameBuffer  := 0;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.Initialize(hDC: THandle;
                                            factor: NativeInt;
                           transparent, supportGDI: Boolean): Boolean;
begin
    // no device context to render to?
    if (hDC = 0) then
    begin
        Result := False;
        Exit;
    end;

    // no owner?
    if (not Assigned(m_pOwner)) then
    begin
        Result := False;
        Exit;
    end;

    // no renderer?
    if (not Assigned(m_pRenderer)) then
    begin
        Result := False;
        Exit;
    end;

    // release previous instance if exists
    Release(hDC);

    m_Factor      := factor;
    m_Transparent := transparent;

    // is alpha blending or antialiasing enabled?
    if (m_Transparent or (m_Factor <> 1)) then
    begin
        // start OpenGL instance
        if (not m_pRenderer.EnableOpenGL(True, hDC, m_hGLContext)) then
        begin
            TQRLogHelper.LogToCompiler('TQRVCLModelRenderSurfaceGL - FAILED - Could not create render context');
            Release(hDC);
            Result := False;
            Exit;
        end;

        // check if OpenGL Extension is already initialized. If not, try to initialize it
        if (not TQRVCLOpenGLHelper.InitializeOpenGL) then
        begin
            TQRLogHelper.LogToCompiler('TQRVCLModelRenderSurfaceGL - FAILED - Could not initialize the OpenGL Extension module');
            Release(hDC);
            Result := False;
            Exit;
        end;

        m_Allowed := True;

        // update surface size
        m_Width  := m_pOwner.ClientWidth;
        m_Height := m_pOwner.ClientHeight;

        // create ARGB render buffers
        if (not CreateARGBRenderBuffers(hDC, m_Width * m_Factor, m_Height * m_Factor)) then
        begin
            TQRLogHelper.LogToCompiler('TQRVCLModelRenderSurfaceGL - FAILED - Could not create ARGB surface');
            Release(hDC);
            Result := False;
            Exit;
        end;

        Result := True;
        Exit;
    end;

    // owner handle still not allocated?
    if (not m_pOwner.HandleAllocated) then
    begin
        Release(hDC);
        Result := False;
        Exit;
    end;

    // start OpenGL instance
    if (not m_pRenderer.EnableOpenGL(not supportGDI, hDC, m_hGLContext)) then
    begin
        TQRLogHelper.LogToCompiler('TQRVCLModelRenderSurfaceGL - FAILED - Could not create render context');
        Release(hDC);
        Result := False;
        Exit;
    end;

    // check if OpenGL Extension is already initialized. If not, try to initialize it
    if (not TQRVCLOpenGLHelper.InitializeOpenGL) then
    begin
        TQRLogHelper.LogToCompiler('TQRVCLModelRenderSurfaceGL - FAILED - Could not initialize the OpenGL Extension module');
        Release(hDC);
        Result := False;
        Exit;
    end;

    m_Allowed := True;
    Result    := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelRenderSurfaceGL.Release(hDC: THandle);
begin
    // no owner?
    if (not Assigned(m_pOwner)) then
        Exit;

    // no renderer?
    if (not Assigned(m_pRenderer)) then
        Exit;

    // OpenGL was initialized correctly and surface is allowed to work?
    if (m_Allowed) then
    begin
        // is alpha blending or antialiasing enabled?
        if (m_Transparent or (m_Factor <> 1)) then
            ClearARGBRenderBuffers(hDC);

        // shutdown OpenGL instance, if needed
        if (m_hGLContext <> 0) then
            m_pRenderer.DisableOpenGL(0, 0, m_hGLContext);
    end;

    // reset values
    m_hGLContext  := 0;
    m_Width       := 0;
    m_Height      := 0;
    m_Factor      := 1;
    m_Transparent := False;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelRenderSurfaceGL.Resize(hDC: THandle);
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
    if (not EnableContext(hDC)) then
        Exit;

    m_Width  := m_pOwner.ClientWidth;
    m_Height := m_pOwner.ClientHeight;

    // create ARGB render buffers if transparency or antialiasing is enabled
    if ((m_Transparent or (m_Factor <> 1)) and
        not CreateARGBRenderBuffers(hDC, m_Width * m_Factor, m_Height * m_Factor))
    then
    begin
        TQRLogHelper.LogToCompiler('TQRVCLModelRenderSurfaceGL - FAILED - Could not create ARGB surface');
        ClearARGBRenderBuffers(hDC);

        // reset size
        m_Width  := 0;
        m_Height := 0;
        Exit;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.EnableContext(hDC: THandle): Boolean;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    // OpenGL should be enabled
    if ((hDC = 0) or (m_hGLContext = 0)) then
    begin
        Result := False;
        Exit;
    end;

    // make render context as OpenGL current context
    Result := wglMakeCurrent(hDC, m_hGLContext);
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.BeginScene(hDC: THandle): Boolean;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    // make render context as OpenGL current context
    if (not EnableContext(hDC)) then
    begin
        Result := False;
        Exit;
    end;

    // configure OpenGL depth testing
    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_TRUE);
    glDepthFunc(GL_LEQUAL);
    glDepthRange(0.0, 1.0);

    // is alpha blending or antialiasing enabled?
    if (m_Transparent or (m_Factor <> 1)) then
        // bind offscreen render buffer to draw scene to
        if (m_OverlayFrameBuffer <> 0) then
            glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_OverlayFrameBuffer);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelRenderSurfaceGL.EndScene(hDC: THandle);
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
        Exit;

    // device context should exists and OpenGL should be enabled
    if ((hDC = 0) or (m_hGLContext = 0)) then
        Exit;

    // is alpha blending or antialiasing enabled?
    if (m_Transparent or (m_Factor <> 1)) then
    begin
        // process OpenGL pending operations
        glFlush;

        // unbind draw buffer, restore default one provided by Windows instead
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    end
    else
        // show render target
        SwapBuffers(hDC);
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderSurfaceGL.GetPixels(hDC: THandle; var pixels: TQRByteArray): Boolean;
begin
    // OpenGL was not initialized correctly and surface is not allowed to work?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    // no owner?
    if (not Assigned(m_pOwner)) then
    begin
        Result := False;
        Exit;
    end;

    // size is empty or does not match with owner?
    if ((m_Width  <= 0)                    or
        (m_Height <= 0)                    or
        (m_Width  <> m_pOwner.ClientWidth) or
        (m_Height <> m_pOwner.ClientHeight))
    then
    begin
        Result := False;
        Exit;
    end;

    // make render context as OpenGL current context
    if (not EnableContext(hDC)) then
    begin
        Result := False;
        Exit;
    end;

    // flush OpenGL
    glFinish;
    glPixelStorei(GL_PACK_ALIGNMENT,   4);
    glPixelStorei(GL_PACK_ROW_LENGTH,  0);
    glPixelStorei(GL_PACK_SKIP_ROWS,   0);
    glPixelStorei(GL_PACK_SKIP_PIXELS, 0);

    // create pixels buffer
    SetLength(pixels, (m_pOwner.ClientWidth * m_Factor) * (m_pOwner.ClientHeight * m_Factor) * 4);

    // is alpha blending or antialiasing enabled?
    if (m_Transparent or (m_Factor <> 1)) then
        // notify that pixels will be read from color buffer
        glReadBuffer(GL_COLOR_ATTACHMENT0);

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
function TQRVCLModelRenderSurfaceGL.GetBitmap(hDC: THandle; pBitmap: Vcl.Graphics.TBitmap): Boolean;
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
    begin
        Result := False;
        Exit;
    end;

    try
        // get OpenGL scene as pixel array
        if (not GetPixels(hDC, pixels)) then
        begin
            Result := False;
            Exit;
        end;

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
                        // search for alpha format to apply
                        case (pBitmap.AlphaFormat) of
                            afIgnored:
                            begin
                                // copy pixel to destination bitmap
                                pLine32[x].rgbRed      := pixels[offset];
                                pLine32[x].rgbGreen    := pixels[offset + 1];
                                pLine32[x].rgbBlue     := pixels[offset + 2];
                                pLine32[x].rgbReserved := 255;
                            end;

                            afDefined:
                            begin
                                // copy pixel to destination bitmap
                                pLine32[x].rgbRed      := pixels[offset];
                                pLine32[x].rgbGreen    := pixels[offset + 1];
                                pLine32[x].rgbBlue     := pixels[offset + 2];
                                pLine32[x].rgbReserved := pixels[offset + 3];
                            end;

                            afPremultiplied:
                            begin
                                alphaValue := pixels[offset + 3];

                                // calculate alpha factor to use to premultiply color components
                                alphaFactor := alphaValue / 255.0;

                                // copy pixel to destination bitmap
                                pLine32[x].rgbRed      := Trunc(alphaFactor * pixels[offset]);
                                pLine32[x].rgbGreen    := Trunc(alphaFactor * pixels[offset + 1]);
                                pLine32[x].rgbBlue     := Trunc(alphaFactor * pixels[offset + 2]);
                                pLine32[x].rgbReserved := Trunc(alphaFactor * pixels[offset + 3]);
                            end;
                        else
                            Result := False;
                            Exit;
                        end;

                        Inc(offset, 4);
                    end;
                end;
            end;
        else
            Result := False;
            Exit;
        end;
    finally
        // clear memory
        SetLength(pixels, 0);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------

end.
