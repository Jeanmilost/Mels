// *************************************************************************************************
// * ==> UTQRModelRenderer ------------------------------------------------------------------------*
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
 @abstract(@name provides a basic interface to implement a model renderer.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRModelRenderer;

interface

uses System.SysUtils,
     System.Math,
     UTQRGeometry,
     UTQR3D;

type
    {$REGION 'Documentation'}
    {**
     Basic interface to implement a model renderer
    }
    {$ENDREGION}
    TQRModelRenderer = class
        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

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
            class function GetOrtho(left,
                                    right,
                                    bottom,
                                    top,
                                    zNear,
                                    zFar: Single): TQRMatrix4x4; static;

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
            class function GetFrustum(left,
                                      right,
                                      bottom,
                                      top,
                                      zNear,
                                      zFar: Single): TQRMatrix4x4; static;

            {$REGION 'Documentation'}
            {**
             Gets projection (or camera) matrix
             @param(fov Field of view, in degrees)
             @param(width View width, generally the same as window width)
             @param(height View height, generally the same as window height)
             @param(zNear Near plane clipping)
             @param(zFar Far plane clipping)
             @return(Matrix)
             @br @bold(NOTE) This function returns the exactly same matrix as gluPerspective
            }
            {$ENDREGION}
            class function GetProjection(fov,
                                         width,
                                         height,
                                         zNear,
                                         zFar: Single): TQRMatrix4x4; static;

            {$REGION 'Documentation'}
            {**
             Creates combined look at matrix (left hand system)
             @param(position Eye (or camera) target position)
             @param(direction Eye (or camera) direction vector)
             @param(up Up vector direction)
             @return(Look at matrix)
            }
            {$ENDREGION}
            class function LookAtLH(const position,
                                         direction,
                                                up: TQRVector3D): TQRMatrix4x4; static;

            {$REGION 'Documentation'}
            {**
             Creates combined look at matrix (right hand system)
             @param(position Eye (or camera) target position)
             @param(direction Eye (or camera) direction vector)
             @param(up Up vector direction)
             @return(Look at matrix)
            }
            {$ENDREGION}
            class function LookAtRH(const position,
                                         direction,
                                                up: TQRVector3D): TQRMatrix4x4; static;

            {$REGION 'Documentation'}
            {**
             Draws a mesh
             @param(mesh Mesh to draw)
             @param(translation Translation to apply to mesh)
             @param(rotationX Rotation on x axis to apply to mesh)
             @param(rotationY Rotation on y axis to apply to mesh)
             @param(rotationZ Rotation on z axis to apply to mesh)
             @param(scale Scaling to apply to mesh)
             @param(textures Model textures)
            }
            {$ENDREGION}
            procedure Draw(var mesh: TQRMesh;
                  const translation: TQRVector3D;
                          rotationX,
                          rotationY,
                          rotationZ: Single;
                        const scale: TQRVector3D;
                     const textures: TQRTextures); overload; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Draws a mesh
             @param(mesh Mesh to draw)
             @param(modelMatrix Model matrix to apply to mesh)
             @param(textures Model textures)
            }
            {$ENDREGION}
            procedure Draw(var mesh: TQRMesh;
                  const modelMatrix: TQRMatrix4x4;
                     const textures: TQRTextures); overload; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Draws a mesh using shader
             @param(mesh Mesh to draw)
             @param(modelMatrix Model matrix to apply to mesh)
             @param(textures Model textures)
             @param(pShader Shader that will be used to draw the model)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Draw(var mesh: TQRMesh;
                 const modelMatrix: TQRMatrix4x4;
                    const textures: TQRTextures;
                           pShader: TQRShader): Boolean; overload; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Draws a mesh using shader
             @param(mesh Mesh to draw)
             @param(nextMesh Mesh to interpolate with)
             @param(modelMatrix Model matrix to apply to mesh)
             @param(interpolationFactor Interpolation factor)
             @param(textures Model textures)
             @param(pShader Shader that will be used to draw the model)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Draw(var mesh: TQRMesh;
                    const nextMesh: TQRMesh;
                 const modelMatrix: TQRMatrix4x4;
               interpolationFactor: Single;
                    const textures: TQRTextures;
                           pShader: TQRShader): Boolean; overload; virtual; abstract;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRModelRenderer
//--------------------------------------------------------------------------------------------------
constructor TQRModelRenderer.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRModelRenderer.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
class function TQRModelRenderer.GetOrtho(left,
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
class function TQRModelRenderer.GetFrustum(left,
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
class function TQRModelRenderer.GetProjection(fov,
                                              width,
                                              height,
                                              zNear,
                                              zFar: Single): TQRMatrix4x4;
var
    aspect, top, bottom, right, left: Single;
begin
    // width or height out of bounds?
    if ((width = 0.0) or (height = 0.0)) then
        raise Exception.Create('Invalid width or height');

    // configure matrix values to use
    aspect :=  width / height;
    top    :=  0.5 * Tan((fov * PI) / 360.0);
    bottom := -top;
    right  :=  aspect * top;
    left   := -right;

    // build and return camera matrix
    Result := GetFrustum(left, right, bottom, top, zNear, zFar);
end;
//--------------------------------------------------------------------------------------------------
class function TQRModelRenderer.LookAtLH(const position, direction, up: TQRVector3D): TQRMatrix4x4;
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
class function TQRModelRenderer.LookAtRH(const position, direction, up: TQRVector3D): TQRMatrix4x4;
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

end.
