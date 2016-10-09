{**************************************************************************************************
 * ==> UTQRModelRenderer -------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides a basic interface to implement a model renderer             *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRModelRenderer;

interface

uses System.SysUtils,
     System.Math,
     UTQRGeometry,
     UTQR3D;

type
    {**
    * Basic interface to implement a model renderer
    *}
    TQRModelRenderer = class
        public
            {**
            * Constructor
            *}
            constructor Create; virtual;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Gets orthogonal projection matrix (glOrtho() OpenGL equivalent)
            *@param left - viewport left edge
            *@param right - viewport right edge
            *@param bottom - viewport bottom edge
            *@param top - viewport top edge
            *@param zNear - near clipping plane
            *@param zFar - far clipping plane
            *@return matrix
            *}
            class function GetOrtho(left,
                                    right,
                                    bottom,
                                    top,
                                    zNear,
                                    zFar: Single): TQRMatrix4x4; static;

            {**
            * Gets frustum projection matrix (glFrustum() OpenGL equivalent)
            *@param left - viewport left edge
            *@param right - viewport right edge
            *@param bottom - viewport bottom edge
            *@param top - viewport top edge
            *@param zNear - near clipping plane
            *@param zFar - far clipping plane
            *@return matrix
            *}
            class function GetFrustum(left,
                                      right,
                                      bottom,
                                      top,
                                      zNear,
                                      zFar: Single): TQRMatrix4x4; static;

            {**
            * Gets projection (or camera) matrix
            *@param fov - field of view, in degrees
            *@param width - view width, generally the same as window width
            *@param height - view height, generally the same as window height
            *@param zNear - near plane clipping
            *@param zFar - far plane clipping
            *@return matrix
            *@note This function returns the exactly same matrix as gluPerspective
            *}
            class function GetProjection(fov,
                                         width,
                                         height,
                                         zNear,
                                         zFar: Single): TQRMatrix4x4; static;

            {**
            * Creates combined look at matrix (left hand system)
            *@param position - eye (or camera) target position
            *@param direction - eye (or camera) direction vector
            *@param up - up vector direction
            *@return look at matrix
            *}
            class function LookAtLH(const position,
                                         direction,
                                                up: TQRVector3D): TQRMatrix4x4; static;

            {**
            * Creates combined look at matrix (right hand system)
            *@param position - eye (or camera) target position
            *@param direction - eye (or camera) direction vector
            *@param up - up vector direction
            *@return look at matrix
            *}
            class function LookAtRH(const position,
                                         direction,
                                                up: TQRVector3D): TQRMatrix4x4; static;

            {**
            * Draws a mesh
            *@param mesh - mesh to draw
            *@param translation - translation to apply to mesh
            *@param rotationX - rotation on x axis to apply to mesh
            *@param rotationY - rotation on y axis to apply to mesh
            *@param rotationZ - rotation on z axis to apply to mesh
            *@param scale - scaling to apply to mesh
            *@param textures - model textures
            *}
            procedure Draw(var mesh: TQRMesh;
                  const translation: TQRVector3D;
                          rotationX,
                          rotationY,
                          rotationZ: Single;
                        const scale: TQRVector3D;
                     const textures: TQRTextures); overload; virtual; abstract;

            {**
            * Draws a mesh
            *@param mesh - mesh to draw
            *@param modelMatrix - model matrix to apply to mesh
            *@param textures - model textures
            *}
            procedure Draw(var mesh: TQRMesh;
                  const modelMatrix: TQRMatrix4x4;
                     const textures: TQRTextures); overload; virtual; abstract;

            {**
            * Draws a mesh using shader
            *@param mesh - mesh to draw
            *@param modelMatrix - model matrix to apply to mesh
            *@param textures - model textures
            *@param pShader - shader that will be used to draw the model
            *@return true on success, otherwise false
            *}
            function Draw(var mesh: TQRMesh;
                 const modelMatrix: TQRMatrix4x4;
                    const textures: TQRTextures;
                           pShader: TQRShader): Boolean; overload; virtual; abstract;

            {**
            * Draws a mesh using shader
            *@param mesh - mesh to draw
            *@param nextMesh - mesh to interpolate with
            *@param modelMatrix - model matrix to apply to mesh
            *@param interpolationFactor - interpolation factor
            *@param textures - model textures
            *@param pShader - shader that will be used to draw the model
            *@return true on success, otherwise false
            *}
            function Draw(var mesh: TQRMesh;
                    const nextMesh: TQRMesh;
                 const modelMatrix: TQRMatrix4x4;
               interpolationFactor: Single;
                    const textures: TQRTextures;
                           pShader: TQRShader): Boolean; overload; virtual; abstract;
    end;

implementation
//------------------------------------------------------------------------------
// TQRModelRenderer
//------------------------------------------------------------------------------
constructor TQRModelRenderer.Create;
begin
    inherited Create;
end;
//------------------------------------------------------------------------------
destructor TQRModelRenderer.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
class function TQRModelRenderer.GetOrtho(left,
                                         right,
                                         bottom,
                                         top,
                                         zNear,
                                         zFar: Single): TQRMatrix4x4;
var
    prl, mrl, mlr, ptb, mtb, mbt, pfn, mnf: Single;
begin
    // OpenGL specifications                             can be rewritten as
    // |  2/(r-l) 0        0       -(r+l)/(r-l) |        |  2/(r-l) 0       0       (r+l)/(l-r) |
    // |  0       2/(t-b)  0       -(t+b)/(t-b) |   =>   |  0       2/(t-b) 0       (t+b)/(b-t) |
    // |  0       0       -2/(f-n) -(f+n)/(f-n) |        |  0       0       2/(n-f) (f+n)/(n-f) |
    // |  0       0        0        1           |        |  0       0       0       1           |
    // invalid for n <= 0, f <= 0, l = r, b = t, or n = f

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
//------------------------------------------------------------------------------
class function TQRModelRenderer.GetFrustum(left,
                                           right,
                                           bottom,
                                           top,
                                           zNear,
                                           zFar: Single): TQRMatrix4x4;
var
    x2n, x2nf, pfn, mnf, prl, mrl, ptb, mtb: Single;
begin
    // OpenGL specifications                                     can be rewritten as
    // |  2n/(r-l)  0          (r+l)/(r-l)   0          |        |  2n/(r-l)  0          (r+l)/(r-l)  0          |
    // |  0         2n/(t-b)   (t+b)/(t-b)   0          |   =>   |  0         2n/(t-b)   (t+b)/(t-b)  0          |
    // |  0         0         -(f+n)/(f-n)  -2fn/(f-n)  |        |  0         0          (f+n)/(n-f)  2fn/(n-f)  |
    // |  0         0         -1             0          |        |  0         0         -1            0          |
    // invalid for n <= 0, f <= 0, l = r, b = t, or n = f

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
    Result := TQRMatrix4x4.Create(x2n / mrl, 0.0,        prl / mrl, 0.0,
                                  0.0,       x2n / mtb,  ptb / mtb, 0.0,
                                  0.0,       0.0,        pfn / mnf, x2nf / mnf,
                                  0.0,       0.0,       -1.0,       0.0);
end;
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
class function TQRModelRenderer.LookAtLH(const position, direction, up: TQRVector3D): TQRMatrix4x4;
var
    xAxis, yAxis, zAxis: TQRVector3D;
begin
    // compute per axis transformations
    zAxis := direction.Sub(position).Normalize();
    xAxis := up.Cross(zAxis).Normalize();
    yAxis := zAxis.Cross(xAxis);

    // create look at matrix, translate eye position
    Result := TQRMatrix4x4.Create( xAxis.X,              yAxis.X,              zAxis.X,             0.0,
                                   xAxis.Y,              yAxis.Y,              zAxis.Y,             0.0,
                                   xAxis.Z,              yAxis.Z,              zAxis.Z,             0.0,
                                  -xAxis.Dot(position), -yAxis.Dot(position), -zAxis.Dot(position), 1.0);
end;
//------------------------------------------------------------------------------
class function TQRModelRenderer.LookAtRH(const position, direction, up: TQRVector3D): TQRMatrix4x4;
var
    xAxis, yAxis, zAxis: TQRVector3D;
begin
    // compute per axis transformations
    zAxis := direction.Sub(position).Normalize();
    xAxis := up.Cross(zAxis).Normalize();
    yAxis := zAxis.Cross(xAxis);

    // create look at matrix, translate eye position
    Result := TQRMatrix4x4.Create(xAxis.X,             yAxis.X,             zAxis.X,             0.0,
                                  xAxis.Y,             yAxis.Y,             zAxis.Y,             0.0,
                                  xAxis.Z,             yAxis.Z,             zAxis.Z,             0.0,
                                  xAxis.Dot(position), yAxis.Dot(position), zAxis.Dot(position), 1.0);
end;
//------------------------------------------------------------------------------

end.
