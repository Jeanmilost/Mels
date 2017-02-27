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
 @abstract(@name contains the collision detection demo main form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit Main;

{$MODE Delphi}

interface

uses Classes,
     SysUtils,
     Variants,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     Gl,
     UTQR3D,
     UTQRGeometry,
     UTQRCollision,
     UTQRVCLMD2ModelComponentGL,
     UTQRVCLModelRendererGL,
     UTQRVCLModelShaderGL;

type
    {**
     Main form class
    }
    TMainForm = class(TForm)
        published
            m2Model: TQRVCLMD2ModelGL;

            procedure m2ModelDetectCollisions(pSender: TObject;
                               const projectionMatrix,
                                           viewMatrix,
                                          modelMatrix: TQRMatrix4x4;
                                            pAABBTree: TQRAABBTree;
                                            pRenderer: TQRVCLModelRendererGL;
                                              pShader: TQRVCLModelShaderGL);

        private
            {**
             Detect collisions between the mouse pointer and the model and draw polyons in collision
             @param(pSender Event sender)
             @param(projectionMatrix Projection (or word) matrix used to render the model)
             @param(modelMatrix Model matrix)
             @param(pAABBTree Model aligned-axis bounding box tree)
             @param(pRenderer OpenGL renderer)
             @param(pShader OpenGL shader)
            }
            procedure DetectAndDrawCollisions(const projectionMatrix,
                                                          viewMatrix,
                                                         modelMatrix: TQRMatrix4x4;
                                                           pAABBTree: TQRAABBTree;
                                                           pRenderer: TQRVCLModelRendererGL;
                                                             pShader: TQRVCLModelShaderGL);
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R *.lfm}
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
procedure TMainForm.m2ModelDetectCollisions(pSender: TObject;
                             const projectionMatrix,
                                         viewMatrix,
                                        modelMatrix: TQRMatrix4x4;
                                          pAABBTree: TQRAABBTree;
                                          pRenderer: TQRVCLModelRendererGL;
                                            pShader: TQRVCLModelShaderGL);
begin
    DetectAndDrawCollisions(projectionMatrix, viewMatrix, modelMatrix, pAABBTree, pRenderer, pShader);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.DetectAndDrawCollisions(const projectionMatrix,
                                                        viewMatrix,
                                                       modelMatrix: TQRMatrix4x4;
                                                         pAABBTree: TQRAABBTree;
                                                         pRenderer: TQRVCLModelRendererGL;
                                                           pShader: TQRVCLModelShaderGL);
var
    rect:                                              TQRRect;
    rayPos, rayDir:                                    TQRVector3D;
    pRay:                                              TQRRay;
    polygons, polygonToDraw:                           TQRPolygons;
    polygon:                                           TQRPolygon;
    mesh:                                              TQRMesh;
    textures:                                          TQRTextures;
    invertProj, invertView, invertModel, invertMatrix: TQRMatrix4x4;
    determinant:                                       Single;
    polygonCount, offset:                              NativeUInt;
begin
    if (not Assigned(pAABBTree)) then
        Exit;

    // calculate client rect in OpenGL coordinates
    rect := TQRRect.Create(-1.0, 1.0, 2.0, 2.0);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    rayPos := pRenderer.MousePosToGLPoint(Handle, rect);
    rayDir := TQRVector3D.Create(0.0, 0.0, 1.0);

    // move the ray to match with the model coordinates
    rayPos.Y := rayPos.Y + 0.05;

    // this is a lazy way to correct a perspective issue. In fact, the model is much larger than its
    // image on the screen, but it is placed very far in relation to the screen. In the model
    // coordinates, the ray location is beyond the mouse coordinate. For that, a ratio is needed to
    // keep the ray coordinates coherent with the mouse position. Not ideal (e.g. the model feet are
    // not always well detected), but this is efficient for the majority of cases
    rayPos.MulAndAssign(1.1);

    // transform the ray to be on the same coordinates system as the model
    invertProj   := projectionMatrix.Inverse(determinant);
    invertView   := viewMatrix.Inverse(determinant);
    invertModel  := modelMatrix.Inverse(determinant);
    invertMatrix := invertProj.Multiply(invertView.Multiply(invertModel));
    rayPos       := invertMatrix.Transform(rayPos);
    rayDir       := invertMatrix.Transform(rayDir);

    // create and populate ray from mouse position
    pRay     := TQRRay.Create;
    pRay.Pos := @rayPos;
    pRay.Dir := @rayDir;

    // get polygons to check for collision by resolving AABB tree
    pAABBTree.Resolve(pRay, polygons);

    // iterate through polygons to check
    for polygon in polygons do
        // is polygon intersecting ray?
        if (TQRCollisionHelper.GetRayPolygonCollision(pRay, polygon)) then
        begin
            // add polygon in collision to resulting list
            SetLength(polygonToDraw, Length(polygonToDraw) + 1);
            polygonToDraw[Length(polygonToDraw) - 1] := polygon;
        end;

    polygonCount := Length(polygonToDraw);

    // found polgons to draw?
    if (polygonCount = 0) then
        Exit;

    try
        // create a vertex buffer and populate it with polygons in collisions
        SetLength(mesh, 1);
        mesh[0].m_Type      := EQR_VT_Triangles;
        mesh[0].m_CoordType := EQR_VC_XYZ;
        mesh[0].m_Stride    := 7;
        Include(mesh[0].m_Format, EQR_VF_Colors);
        SetLength(mesh[0].m_Buffer, polygonCount * (mesh[0].m_Stride * 3));

        offset := 0;

        // iterate through polygons to draw
        for polygon in polygonToDraw do
        begin
            // build polygon to show
            mesh[0].m_Buffer[offset]      := polygon.Vertex1.X;
            mesh[0].m_Buffer[offset + 1]  := polygon.Vertex1.Y;
            mesh[0].m_Buffer[offset + 2]  := polygon.Vertex1.Z;
            mesh[0].m_Buffer[offset + 3]  := 1.0;
            mesh[0].m_Buffer[offset + 4]  := 0.0;
            mesh[0].m_Buffer[offset + 5]  := 0.0;
            mesh[0].m_Buffer[offset + 6]  := 1.0;
            mesh[0].m_Buffer[offset + 7]  := polygon.Vertex2.X;
            mesh[0].m_Buffer[offset + 8]  := polygon.Vertex2.Y;
            mesh[0].m_Buffer[offset + 9]  := polygon.Vertex2.Z;
            mesh[0].m_Buffer[offset + 10] := 0.8;
            mesh[0].m_Buffer[offset + 11] := 0.0;
            mesh[0].m_Buffer[offset + 12] := 0.2;
            mesh[0].m_Buffer[offset + 13] := 1.0;
            mesh[0].m_Buffer[offset + 14] := polygon.Vertex3.X;
            mesh[0].m_Buffer[offset + 15] := polygon.Vertex3.Y;
            mesh[0].m_Buffer[offset + 16] := polygon.Vertex3.Z;
            mesh[0].m_Buffer[offset + 17] := 1.0;
            mesh[0].m_Buffer[offset + 18] := 0.12;
            mesh[0].m_Buffer[offset + 19] := 0.2;
            mesh[0].m_Buffer[offset + 20] := 1.0;

            // go to next polygon
            Inc(offset, mesh[0].m_Stride * 3);
        end;

        glMatrixMode(GL_MODELVIEW);

        glPushMatrix;

        // place triangles into 3D world
        glLoadMatrixf(PGLfloat(modelMatrix.GetPtr));

        // configure OpenGL to draw polygons in collision
        glDisable(GL_TEXTURE_2D);
        glCullFace(GL_NONE);
        glDisable(GL_DEPTH_TEST);

        // draw polygons in collision with mouse pointer
        pRenderer.Draw(mesh, modelMatrix, textures);

        // restore previous OpenGL parameters
        glEnable(GL_DEPTH_TEST);
        glCullFace(GL_FRONT);
        glEnable(GL_TEXTURE_2D);

        glPopMatrix;

        glFlush;
    finally
        if (Length(mesh) > 0) then
        begin
            SetLength(mesh[0].m_Buffer, 0);
            SetLength(mesh, 0);
        end;
    end;

    if (m2Model.NoAnimation) then
       m2Model.Repaint;
end;
//--------------------------------------------------------------------------------------------------

end.
