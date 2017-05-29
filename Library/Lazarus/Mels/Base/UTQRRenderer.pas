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
 @abstract(@name provides a basic interface to implement a renderer.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRRenderer;

interface

uses SysUtils,
     Math,
     UTQRGeometry;

type
    {$REGION 'Documentation'}
    {**
     Basic interface to implement a model renderer
    }
    {$ENDREGION}
    TQRRenderer = class
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
             Gets orthogonal projection matrix (OpenGL glOrtho() equivalent)
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
             Gets frustum projection matrix (OpenGL glFrustum() equivalent)
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
             Unprojects a ray (i.e. transforms it in viewport coordinates)
             @param(projectionMatrix Projection matrix)
             @param(viewMatrix View matrix)
             @param(rayPos @bold([in, out]) Ray position, unprojected ray position on function ends)
             @param(rayDir @bold([in, out]) Ray direction, unprojected ray direction on function ends)
            }
            {$ENDREGION}
            class procedure Unproject(const projectionMatrix, viewMatrix: TQRMatrix4x4;
                                                      var rayPos, rayDir: TQRVector3D); static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRRenderer
//--------------------------------------------------------------------------------------------------
constructor TQRRenderer.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRRenderer.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
class function TQRRenderer.GetOrtho(left,
                                    right,
                                    bottom,
                                    top,
                                    zNear,
                                    zFar: Single): TQRMatrix4x4;
var
    prl, mrl, mlr, ptb, mtb, mbt, pfn, mnf: Single;
begin
    // OpenGL specifications                                 can be rewritten as
    // |  2/(r-l)  0         0        -(r+l)/(r-l)  |        |  2/(r-l)  0        0        (r+l)/(l-r)  |
    // |  0        2/(t-b)   0        -(t+b)/(t-b)  |   =>   |  0        2/(t-b)  0        (t+b)/(b-t)  |
    // |  0        0        -2/(f-n)  -(f+n)/(f-n)  |        |  0        0        2/(n-f)  (f+n)/(n-f)  |
    // |  0        0         0         1            |        |  0        0        0        1            |

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
    Result := TQRMatrix4x4.Create(2.0 / mrl, 0.0,       0.0,       prl / mlr,
                                  0.0,       2.0 / mtb, 0.0,       ptb / mbt,
                                  0.0,       0.0,       2.0 / mnf, pfn / mnf,
                                  0.0,       0.0,       0.0,       1.0);
end;
//--------------------------------------------------------------------------------------------------
class function TQRRenderer.GetFrustum(left,
                                      right,
                                      bottom,
                                      top,
                                      zNear,
                                      zFar: Single): TQRMatrix4x4;
var
    x2n, x2nf, pfn, mnf, prl, mrl, ptb, mtb: Single;
begin
    // OpenGL specifications                                           can be rewritten as
    // |  2n/(r-l)     0             0             0          |        |  2n/(r-l)     0             0            0          |
    // |  0            2n/(t-b)      0             0          |   =>   |  0            2n/(t-b)      0            0          |
    // |  (r+l)/(r-l)  (t+b)/(t-b)  -(f+n)/(f-n)  -2fn/(f-n)  |        |  (r+l)/(r-l)  (t+b)/(t-b)   (f+n)/(n-f)  2fn/(n-f)  |
    // |  0            0            -1             0          |        |  0            0            -1            0          |

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
    Result := TQRMatrix4x4.Create(x2n / mrl, 0.0,        0.0,        0.0,
                                  0.0,       x2n / mtb,  0.0,        0.0,
                                  prl / mrl, ptb / mtb,  pfn  / mnf, x2nf / mnf,
                                  0.0,       0.0,       -1.0,        0.0);
end;
//--------------------------------------------------------------------------------------------------
class function TQRRenderer.GetPerspective(fov,
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
class function TQRRenderer.LookAtLH(const position, direction, up: TQRVector3D): TQRMatrix4x4;
var
    xAxis, yAxis, zAxis: TQRVector3D;
begin
    // compute per axis transformations
    zAxis := direction.Sub(position).Normalize;
    xAxis := up.Cross(zAxis).Normalize;
    yAxis := zAxis.Cross(xAxis);

    // create look at matrix, translate eye position
    Result := TQRMatrix4x4.Create(xAxis.X, xAxis.Y, xAxis.Z, -xAxis.Dot(position),
                                  yAxis.X, yAxis.Y, yAxis.Z, -yAxis.Dot(position),
                                  zAxis.X, zAxis.Y, zAxis.Z, -zAxis.Dot(position),
                                  0.0,     0.0,     0.0,      1.0);
end;
//--------------------------------------------------------------------------------------------------
class function TQRRenderer.LookAtRH(const position, direction, up: TQRVector3D): TQRMatrix4x4;
var
    xAxis, yAxis, zAxis: TQRVector3D;
begin
    // compute per axis transformations
    zAxis := direction.Sub(position).Normalize;
    xAxis := up.Cross(zAxis).Normalize;
    yAxis := zAxis.Cross(xAxis);

    // create look at matrix, translate eye position
    Result := TQRMatrix4x4.Create(xAxis.X, xAxis.Y, xAxis.Z, xAxis.Dot(position),
                                  yAxis.X, yAxis.Y, yAxis.Z, yAxis.Dot(position),
                                  zAxis.X, zAxis.Y, zAxis.Z, zAxis.Dot(position),
                                  0.0,     0.0,     0.0,     1.0);
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRRenderer.Unproject(const projectionMatrix, viewMatrix: TQRMatrix4x4;
                                                      var rayPos, rayDir: TQRVector3D);
var
    determinant:                          Single;
    invertProj, invertView, unprojectMat: TQRMatrix4x4;
begin
    // unproject the ray to make it in the viewport coordinates
    invertProj   := projectionMatrix.Inverse(determinant);
    invertView   := viewMatrix.Inverse(determinant);
    unprojectMat := invertProj.Multiply(invertView);
    rayPos       := unprojectMat.Transform(rayPos);
    rayDir       := unprojectMat.Transform(rayDir);
    rayDir       := rayDir.Normalize();
end;
//--------------------------------------------------------------------------------------------------

end.
