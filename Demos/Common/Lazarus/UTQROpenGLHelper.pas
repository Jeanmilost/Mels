// *************************************************************************************************
// * ==> UTQROpenGLHelper -------------------------------------------------------------------------*
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
 @abstract(@name provides common basic tools to work with OpenGL, as e.g. enabling or disabling
                 OpenGL, or drawing a vertex buffer.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQROpenGLHelper;

interface

uses SysUtils,
     Math,
     Graphics,
     Windows,
     Gl,
     Glu,
     {$IFDEF USE_SHADER}
         OpenGLext,
     {$ENDIF}
     UTQRHelpers,
     UTQR3D,
     UTQRGeometry;

const
    //----------------------------------------------------------------------------------------------
    // Global defines
    //----------------------------------------------------------------------------------------------
    GL_CLAMP_TO_EDGE: NativeUInt = $812F;
    //----------------------------------------------------------------------------------------------

type
    // @exclude(needed, because these data types are ignored and not compiled in Vcl.Graphics if CRL
    //          is defined, although they are useful)
    TQRRGBTripleArray = array [Byte] of TRGBTriple;
    PQRRGBTripleArray = ^TQRRGBTripleArray;
    TQRRGBQuadArray   = array [Byte] of TRGBQuad;
    PQRRGBQuadArray   = ^TQRRGBQuadArray;

    {$REGION 'Documentation'}
    {**
     OpenGL helper, provides common basic tools to work with OpenGL
    }
    {$ENDREGION}
    TQROpenGLHelper = class
        {$REGION 'Documentation'}
        {**
         Enables OpenGL
         @param(hWnd Control or form hanle on which OpenGL scene will be drawn)
         @param(hDC @bold([out]) Device context to use to draw OpenGL scene)
         @param(hRC @bold([out]) Newly created OpenGL context)
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        class function EnableOpenGL(hWnd: THandle;
                                 out hDC: THandle;
                                 out hRC: Thandle): Boolean; static;

        {$REGION 'Documentation'}
        {**
         Disables OpenGL and clears memory
         @param(hWnd Control or form hanle on which OpenGL scene was drawn)
         @param(hDC Device context used to draw OpenGL scene)
         @param(hRC OpenGL context to shutdown)
        }
        {$ENDREGION}
        class procedure DisableOpenGL(hWnd, hDC, hRC: THandle); static;

        {$REGION 'Documentation'}
        {**
         Sets pixel format to use with OpenGL
         @param(hDC Device context used to draw OpenGL scene)
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        class function SetTargetPixelFormat(hDC: THandle): Boolean; static;

        {$REGION 'Documentation'}
        {**
         Creates OpenGL viewport
         @param(clientWidth Form client width)
         @param(clientHeight Form client height)
         @param(createPerspective If @true, perspective matrix will also be created)
        }
        {$ENDREGION}
        class procedure CreateViewport(clientWidth, clientHeight: Integer;
                                               createPerspective: Boolean); static;

        {$REGION 'Documentation'}
        {**
         Gets orthogonal projection matrix (glOrtho() OpenGL equivalent)
         @param(left Viewport left edge)
         @param(right Viewport right edge)
         @param(bottom Viewport bottom edge)
         @param(top Viewport top edge)
         @param(zNear Near clipping plane)
         @param(zFar Far clipping plane)
         @return(Matrix)
        }
        {$ENDREGION}
        class function GetOrtho(left, right, bottom, top, zNear, zFar: Single): TQRMatrix4x4; static;

        {$REGION 'Documentation'}
        {**
         Gets frustum projection matrix (glFrustum() OpenGL equivalent)
         @param(left Viewport left edge)
         @param(right Viewport right edge)
         @param(bottom Viewport bottom edge)
         @param(top Viewport top edge)
         @param(zNear Near clipping plane)
         @param(zFar Far clipping plane)
         @return(Matrix)
        }
        {$ENDREGION}
        class function GetFrustum(left, right, bottom, top, zNear, zFar: Single): TQRMatrix4x4; static;

        {$REGION 'Documentation'}
        {**
         Gets perspective matrix
         @param(fov Field of view, in degrees)
         @param(aspectRatio Aspect ratio, generally width divided by height)
         @param(zNear Near plane clipping)
         @param(zFar Far plane clipping)
         @param(ortho If @true, an orthogonal matrix will be used instead of frustum)
         @return(Matrix)
         @br @bold(NOTE) This function returns the exactly same matrix as gluPerspective
        }
        {$ENDREGION}
        class function GetPerspective(fov,
                              aspectRatio,
                                    zNear,
                                     zFar: Single;
                                    ortho: Boolean = False): TQRMatrix4x4; static;

        {$REGION 'Documentation'}
        {**
         Creates combined look at matrix (left hand system)
         @param(position Eye (or camera) target position)
         @param(direction Eye (or camera) direction vector)
         @param(up Up vector direction)
         @return(Look at matrix)
        }
        {$ENDREGION}
        class function LookAtLH(const position, direction, up: TQRVector3D): TQRMatrix4x4; static;

        {$REGION 'Documentation'}
        {**
         Creates combined look at matrix (right hand system)
         @param(position Eye (or camera) target position)
         @param(direction Eye (or camera) direction vector)
         @param(up Up vector direction)
         @return(Look at matrix)
        }
        {$ENDREGION}
        class function LookAtRH(const position, direction, up: TQRVector3D): TQRMatrix4x4; static;

        {$REGION 'Documentation'}
        {**
         Converts mouse position to OpenGL point (i.e. a point in the OpenGL space)
         @param(hWnd Handle of the window or control on which mouse is hoving)
         @param(viewRect OpenGL view rectangle)
         @return(Converted point in the OpenGL space)
        }
        {$ENDREGION}
        class function MousePosToGLPoint(hWnd: THandle; const viewRect: TQRRect): TQRVector3D; static;

        {$REGION 'Documentation'}
        {**
         Copies the current OpenGL rendering into a bitmap
         @param(pBitmap Bitmap to copy to)
        }
        {$ENDREGION}
        class procedure GetBitmapFromOpenGL(pBitmap: Graphics.TBitmap); static;

        {$REGION 'Documentation'}
        {**
         Converts bitmap in pixels byte array
         @param(pBitmap Bitmap to convert)
         @param(pPixels @bold([out]) Pixels array when function returns, user is responsible to delete it)
         @param(flipY If @true, image will be mirrored on the Y axis)
         @param(bgr If @true, image will be converted from RGB to BGR (or RGBA to ABGR) format)
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        class function BytesFromBitmap(pBitmap: Graphics.TBitmap;
                                   out pPixels: PByte;
                                    flipY, bgr: Boolean): Boolean; static;

        {$REGION 'Documentation'}
        {**
         Creates texture
         @param(width Texture width)
         @param(height Texture height)
         @param(format Texture format, can be GL_RGB or GL_RGBA)
         @param(pPixels Texture pixels array)
         @param(minFiltter Min filter to apply)
         @param(magFilter Mag filter to apply)
         @param(targetID OpenGL target identigier, e.g. GL_TEXTURE_2D)
         @return(Newly created texture identifier)
        }
        {$ENDREGION}
        class function CreateTexture(width, height, format: Integer;
                                                   pPixels: Pointer;
                            minFilter, magFilter, targetID: GLuint): GLint; static;

        {$REGION 'Documentation'}
        {**
         Gets shader uniform hnadle
         @param(pShader Shader containing uniform to get)
         @param(uniform Uniform to get from shader)
         @return(Uniform handle, -1 if not found or on error)
        }
        {$ENDREGION}
        {$IFDEF USE_SHADER}
            class function GetUniform(pShader: TQRShader; uniform: EQRShaderAttribute): GLint; static;
        {$ENDIF}

        {$REGION 'Documentation'}
        {**
         Gets shader attribute
         @param(pShader Shader containing attribute to get)
         @param(attribute Attribute to get from shader)
         @return(Uniform handle, -1 if not found or on error)
        }
        {$ENDREGION}
        {$IFDEF USE_SHADER}
            class function GetAttribute(pShader: TQRShader;
                                      attribute: EQRShaderAttribute): GLint; static;
        {$ENDIF}

        {$REGION 'Documentation'}
        {**
         Draws a mesh using OpenGL
         @param(mesh Mesh to draw
         @param(translation Translation to apply to mesh)
         @param(rotationX Rotation on x axis to apply to mesh)
         @param(rotationY Rotation on y axis to apply to mesh)
         @param(rotationZ Rotation on z axis to apply to mesh)
         @param(scale Scaling to apply to mesh)
         @param(textures Model textures)
        }
        {$ENDREGION}
        class procedure Draw(const mesh: TQRMesh;
                      const translation: TQRVector3D;
                              rotationX,
                              rotationY,
                              rotationZ: Single;
                            const scale: TQRVector3D;
                         const textures: TQRTextures); overload; static;

        {$REGION 'Documentation'}
        {**
         Draws a mesh using OpenGL
         @param(mesh Mesh to draw)
         @param(modelMatrix Model matrix to apply to mesh)
         @param(textures Model textures)
        }
        {$ENDREGION}
        class procedure Draw(const mesh: TQRMesh;
                      const modelMatrix: TQRMatrix4x4;
                         const textures: TQRTextures); overload; static;

        {$REGION 'Documentation'}
        {**
         Draws a mesh using OpenGL and shader
         @param(mesh Mesh to draw)
         @param(modelMatrix Model matrix to apply to mesh)
         @param(textures Model textures)
         @param(pShader Shader that will be used to draw the model)
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        {$IFDEF USE_SHADER}
            class function Draw(const mesh: TQRMesh;
                         const modelMatrix: TQRMatrix4x4;
                            const textures: TQRTextures;
                                   pShader: TQRShader): Boolean; overload; static;
        {$ENDIF}

        {$REGION 'Documentation'}
        {**
         Draws a mesh using OpenGL and shader
         @param(mesh Mesh to draw)
         @param(nextMesh Mesh to interpolate with)
         @param(modelMatrix Model matrix to apply to mesh)
         @param(interpolationFactor Interpolation factor)
         @param(textures Model textures)
         @param(pShader Shader that will be used to draw the model)
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        {$IFDEF USE_SHADER}
            class function Draw(const mesh, nextMesh: TQRMesh;
                                   const modelMatrix: TQRMatrix4x4;
                                 interpolationFactor: Single;
                                      const textures: TQRTextures;
                                             pShader: TQRShader): Boolean; overload; static;
        {$ENDIF}

        {$REGION 'Documentation'}
        {**
         Selects texture to draw
         @param(textures Model texture list)
         @param(modelName Model name to draw (should match with a texture name in the list))
        }
        {$ENDREGION}
        class procedure SelectTexture(const textures: TQRTextures;
                                     const modelName: UnicodeString); overload; static;

        {$REGION 'Documentation'}
        {**
        * Selects texture to draw
        *@param pShader - shader that will draw the texture
        *@param textures - model texture list
        *@param modelName - model name to draw (should match with a texture name in the list)
        }
        {$ENDREGION}
        {$IFDEF USE_SHADER}
            class procedure SelectTexture(pShader: TQRShader;
                                   const textures: TQRTextures;
                                  const modelName: UnicodeString); overload; static;
        {$ENDIF}
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQROpenGLHelper
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.EnableOpenGL(hWnd: THandle;
                                         out hDC: THandle;
                                         out hRC: Thandle): Boolean;
begin
    // no window handle?
    if (hWnd = 0) then
    begin
        Result := False;
        Exit;
    end;

    // get the device context (DC)
    hDC := GetDC(hWnd);

    // failed to get device context?
    if (hDC = 0) then
    begin
        Result := False;
        Exit;
    end;

    // configure pixel format
    if (not SetTargetPixelFormat(hDC)) then
    begin
        Result := False;
        Exit;
    end;

    // create OpenGL render context
    hRC := wglCreateContext(hDC);

    // succeeded?
    if (hRC = 0) then
    begin
        Result := False;
        Exit;
    end;

    // enable OpenGL render context
    if (not wglMakeCurrent(hDC, hRC)) then
    begin
        Result := False;
        Exit;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQROpenGLHelper.DisableOpenGL(hWnd, hDC, hRC: THandle);
begin
    // disable and delete OpenGL context
    if (hRC <> 0) then
    begin
        wglMakeCurrent(0, 0);
        wglDeleteContext(hRC);
    end;

    // delete device context
    if ((hWnd <> 0) and (hDC <> 0)) then
        ReleaseDC(hWnd, hDC);
end;
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.SetTargetPixelFormat(hDC: THandle): Boolean;
var
    pfd:         PIXELFORMATDESCRIPTOR;
    pixelFormat: Integer;
begin
    // no device context?
    if (hDC = 0) then
    begin
        Result := False;
        Exit;
    end;

    with pfd do
    begin
        nSize           := SizeOf(PIXELFORMATDESCRIPTOR);
        nVersion        := 1;
        dwFlags         := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
        iPixelType      := PFD_TYPE_RGBA;
        cColorBits      := 24;
        cRedBits        := 0;
        cRedShift       := 0;
        cGreenBits      := 0;
        cGreenShift     := 0;
        cBlueBits       := 0;
        cBlueShift      := 0;
        cAlphaBits      := 0;
        cAlphaShift     := 0;
        cAccumBits      := 0;
        cAccumRedBits   := 0;
        cAccumGreenBits := 0;
        cAccumBlueBits  := 0;
        cAccumAlphaBits := 0;
        cDepthBits      := 32;
        cStencilBits    := 0;
        cAuxBuffers     := 0;
        iLayerType      := PFD_MAIN_PLANE;
        bReserved       := 0;
        dwLayerMask     := 0;
        dwVisibleMask   := 0;
        dwDamageMask    := 0;
    end;

    // get best available pixel format
    pixelFormat := ChoosePixelFormat(hDC, @pfd);

    // set pixel format to use
    Result := SetPixelFormat(hDC, pixelFormat, @pfd);
end;
//--------------------------------------------------------------------------------------------------
class procedure TQROpenGLHelper.CreateViewport(clientWidth, clientHeight: Integer;
                                                       createPerspective: Boolean);
var
    aspect, width, height: GLfloat;
begin
    // invalid width?
    if (clientWidth = 0) then
        clientWidth := 1;

    // invalid height?
    if (clientHeight = 0) then
        clientHeight := 1;

    // set viewport
    glViewport(0, 0, clientWidth, clientHeight);

    if (not createPerspective) then
        Exit;

    // load projection matrix and initialize it
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;

    width  := clientWidth;
    height := clientHeight;

    // apply perspective correction
    aspect := width / height;
    gluPerspective(45.0, aspect, 0.1, 10000.0);

    // load model view matrix and initialize it
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
end;
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.GetOrtho(left,
                                        right,
                                        bottom,
                                        top,
                                        zNear,
                                        zFar: Single): TQRMatrix4x4;
var
    prl, mrl, mlr, ptb, mtb, mbt, pfn, mnf: Single;
begin
    // OpenGL specifications                                    can be rewritten as
    // |   2/(r-l)       0             0            0  |        |  2/(r-l)      0            0            0  |
    // |   0             2/(t-b)       0            0  |   =>   |  0            2/(t-b)      0            0  |
    // |   0             0            -2/(f-n)      0  |        |  0            0            2/(n-f)      0  |
    // |  -(r+l)/(r-l)  -(t+b)/(t-b)  -(f+n)/(f-n)  1  |        |  (r+l)/(l-r)  (t+b)/(b-t)  (f+n)/(n-f)  1  |

    // are input values out of bounds?
    if ((left = right) or (bottom = top) or (zNear = zFar)) then
        raise Exception.Create('Incorrect input values - cannot create orthogonal matrix');

    // calculate matrix component values
    prl := right  + left;
    mrl := right  - left;
    mlr := left   - right;
    ptb := top    + bottom;
    mtb := top    - bottom;
    mbt := bottom - top;
    pfn := zFar   + zNear;
    mnf := zNear  - zFar;

    // build matrix
    Result := TQRMatrix4x4.Create(2.0 / mrl, 0.0,       0.0,       0.0,
                                  0.0,       2.0 / mtb, 0.0,       0.0,
                                  0.0,       0.0,       2.0 / mnf, 0.0,
                                  prl / mlr, ptb / mbt, pfn / mnf, 1.0);
end;
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.GetFrustum(left,
                                          right,
                                          bottom,
                                          top,
                                          zNear,
                                          zFar: Single): TQRMatrix4x4;
var
    x2n, x2nf, pfn, mnf, prl, mrl, ptb, mtb: Single;
begin
    // OpenGL specifications                                   can be rewritten as
    // |  2n/(r-l)     0             0             0  |        |  2n/(r-l)     0            0             0  |
    // |  0            2n/(t-b)      0             0  |   =>   |  0            2n/(t-b)     0             0  |
    // |  (r+l)/(r-l)  (t+b)/(t-b)  -(f+n)/(f-n)  -1  |        |  (r+l)/(r-l)  (t+b)/(t-b)  (f+n)/(n-f)  -1  |
    // |  0            0            -2fn/(f-n)     0  |        |  0            0            2fn/(n-f)     0  |

    // are input values out of bounds?
    if ((zNear <= 0.0) or (zFar <= 0.0) or (left = right) or (bottom = top) or (zNear = zFar)) then
        raise Exception.Create('Incorrect input values - cannot create frustum matrix');

    // calculate matrix component values
    x2n  := 2.0   * zNear;
    x2nf := x2n   * zFar;
    pfn  := zFar  + zNear;
    mnf  := zNear - zFar;
    prl  := right + left;
    mrl  := right - left;
    ptb  := top   + bottom;
    mtb  := top   - bottom;

    // build matrix
    Result := TQRMatrix4x4.Create(x2n / mrl, 0.0,       0.0,         0.0,
                                  0.0,       x2n / mtb, 0.0,         0.0,
                                  prl / mrl, ptb / mtb, pfn  / mnf, -1.0,
                                  0.0,       0.0,       x2nf / mnf,  0.0);
end;
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.GetPerspective(fov,
                                      aspectRatio,
                                            zNear,
                                             zFar: Single;
                                            ortho: Boolean): TQRMatrix4x4;
var
    maxX, maxY: Single;
begin
    maxY := zNear * Tan(fov * PI / 360.0);
    maxX := maxY  * aspectRatio;

    // do use orthogonal perspective?
    if (ortho) then
        Result := GetOrtho(-maxX, maxX, -maxY, maxY, zNear, zFar)
    else
        Result := GetFrustum(-maxX, maxX, -maxY, maxY, zNear, zFar);
end;
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.LookAtLH(const position, direction, up: TQRVector3D): TQRMatrix4x4;
var
    xAxis, yAxis, zAxis: TQRVector3D;
begin
    // compute per axis transformations
    zAxis := direction.Sub(position).Normalize;
    xAxis := up.Cross(zAxis).Normalize;
    yAxis := zAxis.Cross(xAxis);

    // create look at matrix, translate eye position
    Result := TQRMatrix4x4.Create( xAxis.X,              yAxis.X,              zAxis.X,             0.0,
                                   xAxis.Y,              yAxis.Y,              zAxis.Y,             0.0,
                                   xAxis.Z,              yAxis.Z,              zAxis.Z,             0.0,
                                  -xAxis.Dot(position), -yAxis.Dot(position), -zAxis.Dot(position), 1.0);
end;
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.LookAtRH(const position, direction, up: TQRVector3D): TQRMatrix4x4;
var
    xAxis, yAxis, zAxis: TQRVector3D;
begin
    // compute per axis transformations
    zAxis := direction.Sub(position).Normalize;
    xAxis := up.Cross(zAxis).Normalize;
    yAxis := zAxis.Cross(xAxis);

    // create look at matrix, translate eye position
    Result := TQRMatrix4x4.Create(xAxis.X,             yAxis.X,             zAxis.X,             0.0,
                                  xAxis.Y,             yAxis.Y,             zAxis.Y,             0.0,
                                  xAxis.Z,             yAxis.Z,             zAxis.Z,             0.0,
                                  xAxis.Dot(position), yAxis.Dot(position), zAxis.Dot(position), 1.0);
end;
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.MousePosToGLPoint(hWnd: THandle; const viewRect: TQRRect): TQRVector3D;
var
    p:                                         TPoint;
    clientRect:                                TRect;
    mouseX, mouseY, clientWidth, clientHeight: Single;
begin
    // no window or control?
    if (hWnd = 0) then
    begin
        Result := Default(TQRVector3D);
        Exit;
    end;

    // get mouse position
    GetCursorPos(p);

    // convert to window or control client coordinates
    if (not ScreenToClient(hWnd, p)) then
    begin
        Result := Default(TQRVector3D);
        Exit;
    end;

    mouseX := p.x;
    mouseY := p.y;

    // get window or control client rect
    GetClientRect(hWnd, clientRect);

    // calculate client width and height
    clientWidth  := (clientRect.right  - clientRect.left);
    clientHeight := (clientRect.bottom - clientRect.top);

    // invalid client width or height?
    if ((clientWidth = 0.0) or (clientHeight = 0.0)) then
    begin
        Result := Default(TQRVector3D);
        Exit;
    end;

    // convert mouse position to OpenGL position
    Result := TQRVector3D.Create(viewRect.Min^.X + ((mouseX * viewRect.Width)  / clientWidth),
                                 viewRect.Min^.Y - ((mouseY * viewRect.Height) / clientHeight),
                                 0.0);
end;
//--------------------------------------------------------------------------------------------------
class procedure TQROpenGLHelper.GetBitmapFromOpenGL(pBitmap: Graphics.TBitmap);
var
    dimensions:     array [0..3] of GLint;
    pPixels, pLine: PQRRGBQuadArray;
    x, y, yPos:     GLint;
begin
    // no bitmap?
    if (not Assigned(pBitmap)) then
        Exit;

    // get viewport dimensions
    glGetIntegerv(GL_VIEWPORT, PGLInt(@dimensions));

    // failed?
    if ((dimensions[2] <= 0) or (dimensions[3] <= 0)) then
        Exit;

    pPixels := nil;

    try
        // create bits to contain bitmap
        GetMem(pPixels, dimensions[2] * dimensions[3] * 4);

        // flush OpenGL
        glFinish;
        glPixelStorei(GL_PACK_ALIGNMENT,   4);
        glPixelStorei(GL_PACK_ROW_LENGTH,  0);
        glPixelStorei(GL_PACK_SKIP_ROWS,   0);
        glPixelStorei(GL_PACK_SKIP_PIXELS, 0);

        // get pixels from last OpenGL rendering
        glReadPixels(0, 0, dimensions[2], dimensions[3], GL_RGBA, GL_UNSIGNED_BYTE, pPixels);

        // configure destination bitmap
        pBitmap.PixelFormat := pf32bit;
        pBitmap.SetSize(dimensions[2], dimensions[3]);

        // iterate through lines to copy
        for y := 0 to dimensions[3] - 1 do
        begin
            // get next line to copy and calculate y position (origin is on the left bottom on the
            // source, but on the left top on the destination)
            pLine := PQRRGBQuadArray(pBitmap.ScanLine[y]);
            yPos  := ((dimensions[3] - 1) - y) * dimensions[2];

            // iterate through pixels to copy
            for x := 0 to dimensions[2] - 1 do
            begin
                // take the opportunity to swap the pixel RGB values
                (pLine^)[x].rgbRed      := (pPixels^)[yPos + x].rgbBlue;
                (pLine^)[x].rgbGreen    := (pPixels^)[yPos + x].rgbGreen;
                (pLine^)[x].rgbBlue     := (pPixels^)[yPos + x].rgbRed;
                (pLine^)[x].rgbReserved := (pPixels^)[yPos + x].rgbReserved;
            end;
        end;
    finally
        if (Assigned(pPixels)) then
            FreeMem(pPixels);
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.BytesFromBitmap(pBitmap: Graphics.TBitmap;
                                           out pPixels: PByte;
                                            flipY, bgr: Boolean): Boolean;
var
    width, height, x, y:                  Integer;
    pixelSize, lineSize, offset, offsetX: NativeUInt;
    pLineRGB:                             PQRRGBTripleArray;
    pLineRGBA:                            PQRRGBQuadArray;
begin
    // no bitmap?
    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // is bitmap empty?
    if ((pBitmap.Width <= 0) or (pBitmap.Height <= 0)) then
    begin
        Result := False;
        Exit;
    end;

    // get bitmap size
    width  := pBitmap.Width;
    height := pBitmap.Height;

    // get pixel size
    if (pBitmap.PixelFormat = pf32bit) then
        pixelSize := SizeOf(TRGBQuad)
    else
        pixelSize := SizeOf(TRGBTriple);

    // calculate line size
    lineSize := NativeUInt(width) * pixelSize;

    // create pixels buffer
    GetMem(pPixels, NativeUInt(height) * lineSize);

    // iterate through bitmap lines
    for y := 0 to height - 1 do
    begin
        // calculate next offset
        if (flipY) then
            offset := NativeUInt((height - 1) - y) * lineSize
        else
            offset := NativeUInt(y) * lineSize;

        // is 24 or 32 bit bitmap?
        if (pBitmap.PixelFormat = pf24bit) then
        begin
            // get pixels line from bitmap
            pLineRGB := PQRRGBTripleArray(pBitmap.ScanLine[y]);

            // do swap pixels?
            if (bgr) then
                // memory copy 24 bit pixels line, as pixels are already in RGB format
                CopyMemory(Pointer(NativeUInt(pPixels) + offset), pLineRGB, lineSize)
            else
                // iterate through line pixels
                for x := 0 to width - 1 do
                begin
                    // calculate next pixel offset
                    offsetX := offset + (NativeUInt(x) * pixelSize);

                    // copy and swap pixel
                    pPixels[offsetX]     := (pLineRGB^)[x].rgbtRed;
                    pPixels[offsetX + 1] := (pLineRGB^)[x].rgbtGreen;
                    pPixels[offsetX + 2] := (pLineRGB^)[x].rgbtBlue;
                end;
        end
        else
        begin
            // get pixels line from bitmap
            pLineRGBA := PQRRGBQuadArray(pBitmap.ScanLine[y]);

            // do swap pixels?
            if (bgr) then
                // memory copy 32 bit pixels line, as pixels are already in RGB format
                CopyMemory(Pointer(NativeUInt(pPixels) + offset), pLineRGBA, lineSize)
            else
                // iterate through line pixels
                for x := 0 to width - 1 do
                begin
                    // calculate next pixel offset
                    offsetX := offset + (NativeUInt(x) * pixelSize);

                    // copy and swap pixel
                    pPixels[offsetX]     := (pLineRGBA^)[x].rgbRed;
                    pPixels[offsetX + 1] := (pLineRGBA^)[x].rgbGreen;
                    pPixels[offsetX + 2] := (pLineRGBA^)[x].rgbBlue;
                    pPixels[offsetX + 3] := (pLineRGBA^)[x].rgbReserved;
                end;
        end;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
class function TQROpenGLHelper.CreateTexture(width, height, format: Integer;
                                                           pPixels: Pointer;
                                    minFilter, magFilter, targetID: GLuint): GLint;
var
    texture: GLuint;
begin
    // create and bind new OpenGL texture
    glGenTextures(1, @texture);
    glBindTexture(targetID, texture);

    // set texture environment parameters
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    // set texture filtering
    glTexParameteri(targetID, GL_TEXTURE_MIN_FILTER, minFilter);
    glTexParameteri(targetID, GL_TEXTURE_MAG_FILTER, magFilter);

    // set texture wrapping mode
    glTexParameteri(targetID, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(targetID, GL_TEXTURE_WRAP_T, GL_REPEAT);

    // generate texture from bitmap data
    glTexImage2D(targetID, 0, format, width, height, 0, format, GL_UNSIGNED_BYTE, pPixels);

    Result := texture;
end;
//--------------------------------------------------------------------------------------------------
{$IFDEF USE_SHADER}
    class function TQROpenGLHelper.GetUniform(pShader: TQRShader; uniform: EQRShaderAttribute): GLint;
    var
        propertyName: AnsiString;
    begin
        // no shader?
        if (not Assigned(pShader)) then
        begin
            Result := -1;
            Exit;
        end;

        // get uniform property name
        propertyName := AnsiString(pShader.GetAttributeName(uniform));

        // found it?
        if (Length(propertyName) = 0) then
        begin
            Result := -1;
            Exit;
        end;

        // get model matrix slot from shader
        Result := glGetUniformLocation(pShader.GetProgramID, PAnsiChar(propertyName));
    end;
{$ENDIF}
//--------------------------------------------------------------------------------------------------
{$IFDEF USE_SHADER}
    class function TQROpenGLHelper.GetAttribute(pShader: TQRShader;
                                              attribute: EQRShaderAttribute): GLint;
    var
        propertyName: AnsiString;
    begin
        // no shader?
        if (not Assigned(pShader)) then
        begin
            Result := -1;
            Exit;
        end;

        // get attribute property name
        propertyName := AnsiString(pShader.GetAttributeName(attribute));

        // found it?
        if (Length(propertyName) = 0) then
        begin
            Result := -1;
            Exit;
        end;

        // get shader interpolation position attribute
        Result := glGetAttribLocation(pShader.GetProgramID, PAnsiChar(propertyName));
    end;
{$ENDIF}
//--------------------------------------------------------------------------------------------------
class procedure TQROpenGLHelper.Draw(const mesh: TQRMesh;
                              const translation: TQRVector3D;
                                      rotationX,
                                      rotationY,
                                      rotationZ: Single;
                                    const scale: TQRVector3D;
                                 const textures: TQRTextures);
var
    count, stride, offset, i: NativeUInt;
begin
    // get mesh count
    count := Length(mesh);

    // no mesh to draw?
    if (count = 0) then
        Exit;

    // calculate stride. As all meshes share the same vertex properties, the first mesh can be used
    // to extract vertex format info
    if (mesh[0].m_CoordType = EQR_VC_XYZ) then
        stride := 3
    else
        stride := 2;

    // do use normals array?
    if (EQR_VF_Normals in mesh[0].m_Format) then
        Inc(stride, 3);

    // do use textures coordinates array?
    if (EQR_VF_TexCoords in mesh[0].m_Format) then
        Inc(stride, 2);

    // do use colors array?
    if (EQR_VF_Colors in mesh[0].m_Format) then
        Inc(stride, 4);

    glMatrixMode(GL_MODELVIEW);

    glPushMatrix;

    // place model into 3D world
    glTranslatef(translation.X, translation.Y, translation.Z);
    glRotatef(TQRMathsHelper.RadToDeg(rotationX), 1.0, 0.0, 0.0);
    glRotatef(TQRMathsHelper.RadToDeg(rotationY), 0.0, 1.0, 0.0);
    glRotatef(TQRMathsHelper.RadToDeg(rotationZ), 0.0, 0.0, 1.0);
    glScalef(scale.X, scale.Y, scale.Z);

    // iterate through vertices to draw
    for i := 0 to count - 1 do
    begin
        SelectTexture(textures, mesh[i].m_Name);

        // bind vertex array
        glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(3,
                        GL_FLOAT,
                        stride * SizeOf(Single),
                        @mesh[i].m_Buffer[0]);

        offset := 3;

        // bind normals array
        if (EQR_VF_Normals in mesh[i].m_Format) then
        begin
            glEnableClientState(GL_NORMAL_ARRAY);
            glNormalPointer(GL_FLOAT,
                            stride * SizeOf(Single),
                            @mesh[i].m_Buffer[offset]);

            Inc(offset, 3);
        end;

        // bind texture coordinates array
        if (EQR_VF_TexCoords in mesh[i].m_Format) then
        begin
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glTexCoordPointer(2,
                              GL_FLOAT,
                              stride * SizeOf(Single),
                              @mesh[i].m_Buffer[offset]);

            Inc(offset, 2);
        end;

        // bind colors array
        if (EQR_VF_Colors in mesh[i].m_Format) then
        begin
            glEnableClientState(GL_COLOR_ARRAY);
            glColorPointer(4,
                           GL_FLOAT,
                           stride * SizeOf(Single),
                           @mesh[i].m_Buffer[offset]);
        end;

        // draw mesh
        case mesh[i].m_Type of
            EQR_VT_Triangles:     glDrawArrays(GL_TRIANGLES,      0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
            EQR_VT_TriangleStrip: glDrawArrays(GL_TRIANGLE_STRIP, 0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
            EQR_VT_TriangleFan:   glDrawArrays(GL_TRIANGLE_FAN,   0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
            EQR_VT_Quads:         glDrawArrays(GL_QUADS,          0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
            EQR_VT_QuadStrip:     glDrawArrays(GL_QUAD_STRIP,     0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
        else
            raise Exception.Create('Unknown vertex type');
        end;

        // unbind vertex array
        glDisableClientState(GL_VERTEX_ARRAY);

        // unbind normals array
        if (EQR_VF_Normals in mesh[i].m_Format) then
            glDisableClientState(GL_NORMAL_ARRAY);

        // unbind texture coordinates array
        if (EQR_VF_TexCoords in mesh[i].m_Format) then
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);

        // unbind colors array
        if (EQR_VF_Colors in mesh[i].m_Format) then
            glDisableClientState(GL_COLOR_ARRAY);

        glFlush;
    end;

    glPopMatrix;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQROpenGLHelper.Draw(const mesh: TQRMesh;
                              const modelMatrix: TQRMatrix4x4;
                                 const textures: TQRTextures);
var
    count, stride, offset, i: NativeUInt;
begin
    // get mesh count
    count := Length(mesh);

    // no mesh to draw?
    if (count = 0) then
        Exit;

    // calculate stride. As all meshes share the same vertex properties, the first mesh can be used
    // to extract vertex format info
    if (mesh[0].m_CoordType = EQR_VC_XYZ) then
        stride := 3
    else
        stride := 2;

    // do use normals array?
    if (EQR_VF_Normals in mesh[0].m_Format) then
        Inc(stride, 3);

    // do use textures coordinates array?
    if (EQR_VF_TexCoords in mesh[0].m_Format) then
        Inc(stride, 2);

    // do use colors array?
    if (EQR_VF_Colors in mesh[0].m_Format) then
        Inc(stride, 4);

    glMatrixMode(GL_MODELVIEW);

    glPushMatrix;

    // place model into 3D world
    glLoadMatrixf(PGLfloat(modelMatrix.GetPtr));

    // iterate through vertices to draw
    for i := 0 to count - 1 do
    begin
        SelectTexture(textures, mesh[i].m_Name);

        // bind vertex array
        glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(3,
                        GL_FLOAT,
                        stride * SizeOf(Single),
                        @mesh[i].m_Buffer[0]);

        offset := 3;

        // bind normals array
        if (EQR_VF_Normals in mesh[i].m_Format) then
        begin
            glEnableClientState(GL_NORMAL_ARRAY);
            glNormalPointer(GL_FLOAT,
                            stride * SizeOf(Single),
                            @mesh[i].m_Buffer[offset]);

            Inc(offset, 3);
        end;

        // bind texture coordinates array
        if (EQR_VF_TexCoords in mesh[i].m_Format) then
        begin
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glTexCoordPointer(2,
                              GL_FLOAT,
                              stride * SizeOf(Single),
                              @mesh[i].m_Buffer[offset]);

            Inc(offset, 2);
        end;

        // bind colors array
        if (EQR_VF_Colors in mesh[i].m_Format) then
        begin
            glEnableClientState(GL_COLOR_ARRAY);
            glColorPointer(4,
                           GL_FLOAT,
                           stride * SizeOf(Single),
                           @mesh[i].m_Buffer[offset]);
        end;

        // draw mesh
        case (mesh[i].m_Type) of
            EQR_VT_Triangles:     glDrawArrays(GL_TRIANGLES,      0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
            EQR_VT_TriangleStrip: glDrawArrays(GL_TRIANGLE_STRIP, 0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
            EQR_VT_TriangleFan:   glDrawArrays(GL_TRIANGLE_FAN,   0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
            EQR_VT_Quads:         glDrawArrays(GL_QUADS,          0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
            EQR_VT_QuadStrip:     glDrawArrays(GL_QUAD_STRIP,     0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
        else
            raise Exception.Create('Unknown vertex type');
        end;

        // unbind vertex array
        glDisableClientState(GL_VERTEX_ARRAY);

        // unbind normals array
        if (EQR_VF_Normals in mesh[i].m_Format) then
            glDisableClientState(GL_NORMAL_ARRAY);

        // unbind texture coordinates array
        if (EQR_VF_TexCoords in mesh[i].m_Format) then
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);

        // unbind colors array
        if (EQR_VF_Colors in mesh[i].m_Format) then
            glDisableClientState(GL_COLOR_ARRAY);

        glFlush;
    end;

    glPopMatrix;
end;
//--------------------------------------------------------------------------------------------------
{$IFDEF USE_SHADER}
    class function TQROpenGLHelper.Draw(const mesh: TQRMesh;
                                 const modelMatrix: TQRMatrix4x4;
                                    const textures: TQRTextures;
                                           pShader: TQRShader): Boolean;
    var
        count, stride, offset, i:                                NativeUInt;
        uniform, posAttrib, normalAttrib, uvAttrib, colorAttrib: GLint;
    begin
        // get mesh count
        count := Length(mesh);

        // no mesh to draw?
        if (count = 0) then
        begin
            Result := False;
            Exit;
        end;

        // no shader program?
        if (not Assigned(pShader)) then
        begin
            Result := False;
            Exit;
        end;

        try
            // bind shader program
            pShader.Use(True);

            // get model matrix slot from shader
            uniform := GetUniform(pShader, EQR_SA_ModelMatrix);

            // found it?
            if (uniform = -1) then
            begin
                Result := False;
                Exit;
            end;

            // connect model matrix to shader
            glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(modelMatrix.GetPtr));

            // get shader position attribute
            posAttrib := GetAttribute(pShader, EQR_SA_Position);

            // found it?
            if (posAttrib = -1) then
            begin
                Result := False;
                Exit;
            end;

            // calculate stride. As all meshes share the same vertex properties, the first mesh can
            // be used to extract vertex format info
            if (mesh[0].m_CoordType = EQR_VC_XYZ) then
                stride := 3
            else
                stride := 2;

            normalAttrib := -1;

            // do use shader normal attribute?
            if (EQR_VF_Normals in mesh[0].m_Format) then
            begin
                // get shader normal attribute
                normalAttrib := GetAttribute(pShader, EQR_SA_Normal);

                // found it?
                if (normalAttrib = -1) then
                begin
                    Result := False;
                    Exit;
                end;

                Inc(stride, 3);
            end;

            uvAttrib := -1;

            // do use shader UV attribute?
            if (EQR_VF_TexCoords in mesh[0].m_Format) then
            begin
                // get shader UV attribute
                uvAttrib := GetAttribute(pShader, EQR_SA_Texture);

                // found it?
                if (uvAttrib = -1) then
                begin
                    Result := False;
                    Exit;
                end;

                // add texture coordinates to stride
                Inc(stride, 2);
            end;

            colorAttrib := -1;

            // do use shader color attribute?
            if (EQR_VF_Colors in mesh[0].m_Format) then
            begin
                // get shader color attribute
                colorAttrib := GetAttribute(pShader, EQR_SA_Color);

                // found it?
                if (colorAttrib = -1) then
                begin
                    Result := False;
                    Exit;
                end;

                // add color to stride
                Inc(stride, 4);
            end;

            // iterate through OpenGL meshes
            for i := 0 to count - 1 do
            begin
                SelectTexture(pShader, textures, mesh[i].m_Name);

                offset := 0;

                // connect vertices to vertex shader position attribute
                glEnableVertexAttribArray(posAttrib);
                glVertexAttribPointer(posAttrib,
                                      3,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * SizeOf(Single),
                                      @mesh[i].m_Buffer[offset]);

                if (mesh[i].m_CoordType = EQR_VC_XYZ) then
                    offset := 3
                else
                    offset := 2;

                // vertex buffer contains normals?
                if (normalAttrib <> -1) then
                begin
                    // connect the vertices to the vertex shader normal attribute
                    glEnableVertexAttribArray(normalAttrib);
                    glVertexAttribPointer(normalAttrib,
                                          3,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * SizeOf(Single),
                                          @mesh[i].m_Buffer[offset]);

                    Inc(offset, 3);
                end;

                // vertex buffer contains texture coordinates?
                if (uvAttrib <> -1) then
                begin
                    // connect the color to the vertex shader vColor attribute and redirect to
                    // the fragment shader
                    glEnableVertexAttribArray(uvAttrib);
                    glVertexAttribPointer(uvAttrib,
                                          2,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * SizeOf(Single),
                                          @mesh[i].m_Buffer[offset]);

                    Inc(offset, 2);
                end;

                // vertex buffer contains colors?
                if (colorAttrib <> -1) then
                begin
                    // connect the color to the vertex shader vColor attribute and redirect to
                    // the fragment shader
                    glEnableVertexAttribArray(colorAttrib);
                    glVertexAttribPointer(colorAttrib,
                                          4,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * SizeOf(Single),
                                          @mesh[i].m_Buffer[offset]);
                end;

                // draw mesh
                case (mesh[i].m_Type) of
                    EQR_VT_Triangles:     glDrawArrays(GL_TRIANGLES,      0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                    EQR_VT_TriangleStrip: glDrawArrays(GL_TRIANGLE_STRIP, 0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                    EQR_VT_TriangleFan:   glDrawArrays(GL_TRIANGLE_FAN,   0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                    EQR_VT_Quads:         glDrawArrays(GL_QUADS,          0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                    EQR_VT_QuadStrip:     glDrawArrays(GL_QUAD_STRIP,     0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                else
                    raise Exception.Create('Unknown vertex type');
                end;
            end;
        finally
            // unbind shader program
            pShader.Use(False);
        end;

        Result := True;
    end;
{$ENDIF}
//--------------------------------------------------------------------------------------------------
{$IFDEF USE_SHADER}
    class function TQROpenGLHelper.Draw(const mesh, nextMesh: TQRMesh;
                                           const modelMatrix: TQRMatrix4x4;
                                         interpolationFactor: Single;
                                              const textures: TQRTextures;
                                                     pShader: TQRShader): Boolean;
    var
        count,
        stride,
        offset,
        i:                   NativeUInt;
        uniform,
        interpolationAttrib,
        posAttrib,
        iPosAttrib,
        normalAttrib,
        iNormalAttrib,
        uvAttrib,
        colorAttrib:         GLint;
    begin
        // get mesh count
        count := Length(mesh);

        // no mesh to draw?
        if (count = 0) then
        begin
            Result := False;
            Exit;
        end;

        // no shader program?
        if (not Assigned(pShader)) then
        begin
            Result := False;
            Exit;
        end;

        try
            // bind shader program
            pShader.Use(True);

            // get model matrix slot from shader
            uniform := GetUniform(pShader, EQR_SA_ModelMatrix);

            // found it?
            if (uniform = -1) then
            begin
                Result := False;
                Exit;
            end;

            // connect model matrix to shader
            glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(modelMatrix.GetPtr));

            // get shader position attribute
            interpolationAttrib := GetUniform(pShader, EQR_SA_Interpolation);

            // found interpolation attribute?
            if (interpolationAttrib = -1) then
            begin
                Result := False;
                Exit;
            end;

            // send interpolation factor to shader program
            glUniform1f(interpolationAttrib, interpolationFactor);

            // get shader position attribute
            posAttrib := GetAttribute(pShader, EQR_SA_Position);

            // found it?
            if (posAttrib = -1) then
            begin
                Result := False;
                Exit;
            end;

            // get shader interpolation position attribute
            iPosAttrib := GetAttribute(pShader, EQR_SA_InterpolationPos);

            // found it?
            if (iPosAttrib = -1) then
            begin
                Result := False;
                Exit;
            end;

            // calculate stride. As all meshes share the same vertex properties, the first mesh can
            // be used to extract vertex format info
            if (mesh[0].m_CoordType = EQR_VC_XYZ) then
                stride := 3
            else
                stride := 2;

            normalAttrib  := -1;
            iNormalAttrib := -1;

            // do use shader normal attribute?
            if (EQR_VF_Normals in mesh[0].m_Format) then
            begin
                // get shader normal attribute
                normalAttrib := GetAttribute(pShader, EQR_SA_Normal);

                // found it?
                if (normalAttrib = -1) then
                begin
                    Result := False;
                    Exit;
                end;

                // get shader normal attribute
                iNormalAttrib := GetAttribute(pShader, EQR_SA_InterpolationNormal);

                // found it?
                if (iNormalAttrib = -1) then
                begin
                    Result := False;
                    Exit;
                end;

                Inc(stride, 3);
            end;

            uvAttrib := -1;

            // do use shader UV attribute?
            if (EQR_VF_TexCoords in mesh[0].m_Format) then
            begin
                // get shader UV attribute
                uvAttrib := GetAttribute(pShader, EQR_SA_Texture);

                // found it?
                if (uvAttrib = -1) then
                begin
                    Result := False;
                    Exit;
                end;

                // add texture coordinates to stride
                Inc(stride, 2);
            end;

            colorAttrib := -1;

            // do use shader color attribute?
            if (EQR_VF_Colors in mesh[0].m_Format) then
            begin
                // get shader color attribute
                colorAttrib := GetAttribute(pShader, EQR_SA_Color);

                // found it?
                if (colorAttrib = -1) then
                begin
                    Result := False;
                    Exit;
                end;

                // add color to stride
                Inc(stride, 4);
            end;

            // iterate through OpenGL meshes
            for i := 0 to count - 1 do
            begin
                SelectTexture(pShader, textures, mesh[i].m_Name);

                offset := 0;

                // connect vertices to vertex shader position attribute
                glEnableVertexAttribArray(posAttrib);
                glVertexAttribPointer(posAttrib,
                                      3,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * SizeOf(Single),
                                      @mesh[i].m_Buffer[offset]);

                // connect vertices to vertex shader position attribute
                glEnableVertexAttribArray(iPosAttrib);
                glVertexAttribPointer(iPosAttrib,
                                      3,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * SizeOf(Single),
                                      @nextMesh[i].m_Buffer[offset]);

                if (mesh[i].m_CoordType = EQR_VC_XYZ) then
                    offset := 3
                else
                    offset := 2;

                // vertex buffer contains normals?
                if (normalAttrib <> -1) then
                begin
                    // connect the normals to the vertex shader normal attribute
                    glEnableVertexAttribArray(normalAttrib);
                    glVertexAttribPointer(normalAttrib,
                                          3,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * SizeOf(Single),
                                          @mesh[i].m_Buffer[offset]);

                    // vertex buffer contains interpolated normals?
                    if (iNormalAttrib <> -1) then
                    begin
                        // connect the interpolated normals to the vertex shader normal attribute
                        glEnableVertexAttribArray(iNormalAttrib);
                        glVertexAttribPointer(iNormalAttrib,
                                              3,
                                              GL_FLOAT,
                                              GL_FALSE,
                                              stride * SizeOf(Single),
                                              @nextMesh[i].m_Buffer[offset]);
                    end;

                    Inc(offset, 3);
                end;

                // vertex buffer contains texture coordinates?
                if (uvAttrib <> -1) then
                begin
                    // connect the color to the vertex shader vColor attribute and redirect to
                    // the fragment shader
                    glEnableVertexAttribArray(uvAttrib);
                    glVertexAttribPointer(uvAttrib,
                                          2,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * SizeOf(Single),
                                          @mesh[i].m_Buffer[offset]);

                    Inc(offset, 2);
                end;

                // vertex buffer contains colors?
                if (colorAttrib <> -1) then
                begin
                    // connect the color to the vertex shader vColor attribute and redirect to
                    // the fragment shader
                    glEnableVertexAttribArray(colorAttrib);
                    glVertexAttribPointer(colorAttrib,
                                          4,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * SizeOf(Single),
                                          @mesh[i].m_Buffer[offset]);
                end;

                // draw mesh
                case (mesh[i].m_Type) of
                    EQR_VT_Triangles:     glDrawArrays(GL_TRIANGLES,      0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                    EQR_VT_TriangleStrip: glDrawArrays(GL_TRIANGLE_STRIP, 0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                    EQR_VT_TriangleFan:   glDrawArrays(GL_TRIANGLE_FAN,   0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                    EQR_VT_Quads:         glDrawArrays(GL_QUADS,          0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                    EQR_VT_QuadStrip:     glDrawArrays(GL_QUAD_STRIP,     0, NativeUInt(Length(mesh[i].m_Buffer)) div stride);
                else
                    raise Exception.Create('Unknown vertex type');
                end;
            end;
        finally
            // unbind shader program
            pShader.Use(False);
        end;

        Result := True;
    end;
{$ENDIF}
//--------------------------------------------------------------------------------------------------
class procedure TQROpenGLHelper.SelectTexture(const textures: TQRTextures;
                                             const modelName: UnicodeString);
var
    index:           NativeInt;
    textureCount, i: NativeUInt;
begin
    textureCount := Length(textures);

    // do draw textures?
    if (textureCount = 0) then
    begin
        glDisable(GL_TEXTURE_2D);
        Exit;
    end;

    index := -1;

    // iterate through textures belonging to model
    for i := 0 to textureCount - 1 do
        // found a texture to draw?
        if (Assigned(textures[i]) and (textures[i].Enabled) and (textures[i].Name = modelName)) then
        begin
            // get texture index
            index := i;
            break;
        end;

    // found texture index to draw?
    if (index >= 0) then
    begin
        // draw texture
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, textures[index].Index);
        Exit;
    end;

    glDisable(GL_TEXTURE_2D);
end;
//--------------------------------------------------------------------------------------------------
{$IFDEF USE_SHADER}
    class procedure TQROpenGLHelper.SelectTexture(pShader: TQRShader;
                                           const textures: TQRTextures;
                                          const modelName: UnicodeString);
    var
        uniform:         GLint;
        index:           NativeInt;
        textureCount, i: NativeUInt;
    begin
        // get color map slot from shader
        uniform := GetUniform(pShader, EQR_SA_ColorMap);

        // found it?
        if (uniform = -1) then
            // nothing to do (some shader may have no texture to handle)
            Exit;

        textureCount := Length(textures);

        // do draw textures?
        if (textureCount = 0) then
        begin
            glDisable(GL_TEXTURE_2D);
            Exit;
        end;

        index := -1;

        // iterate through textures belonging to model
        for i := 0 to textureCount - 1 do
            // found a texture to draw?
            if (Assigned(textures[i]) and (textures[i].Enabled) and (textures[i].Name = modelName)) then
            begin
                // get texture index
                index := i;
                break;
            end;

        // found texture index to draw?
        if (index >= 0) then
        begin
            // draw texture
            glEnable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, textures[index].Index);
            glActiveTexture(GL_TEXTURE0);
            Exit;
        end;

        glDisable(GL_TEXTURE_2D);
    end;
{$ENDIF}
//--------------------------------------------------------------------------------------------------

end.
