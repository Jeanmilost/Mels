unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UTQR3D, UTQRGeometry, UTQRCollision, UTQRVCLModelComponentGL,
  UTQRVCLMD2ModelComponentGL,
  UTQRVCLModelRendererGL, UTQRVCLModelShaderGL, Winapi.OpenGL;

type
  TMainForm = class(TForm)
    m2Model: TQRVCLMD2ModelGL;
    procedure m2ModelDetectCollisions(pSender: TObject; const modelMatrix: TQRMatrix4x4;
      pAABBTree: TQRAABBTree; pRenderer: TQRVCLModelRendererGL; pShader: TQRVCLModelShaderGL);

  private
    procedure DetectAndDrawCollisions(const modelMatrix: TQRMatrix4x4; pAABBTree: TQRAABBTree;
            pRenderer: TQRVCLModelRendererGL; pShader: TQRVCLModelShaderGL);

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.m2ModelDetectCollisions(pSender: TObject; const modelMatrix: TQRMatrix4x4;
  pAABBTree: TQRAABBTree; pRenderer: TQRVCLModelRendererGL; pShader: TQRVCLModelShaderGL);
begin
    DetectAndDrawCollisions(modelMatrix, pAABBTree, pRenderer, pShader);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.DetectAndDrawCollisions(const modelMatrix: TQRMatrix4x4; pAABBTree: TQRAABBTree;
        pRenderer: TQRVCLModelRendererGL; pShader: TQRVCLModelShaderGL);
var
    rect:                                        TQRRect;
    rayPos, rayDir:                              TQRVector3D;
    pRay:                                        TQRRay;
    polygons, polygonToDraw:                     TQRPolygons;
    mesh:                                        TQRMesh;
    textures:                                    TQRTextures;
    invertMatrix:                                TQRMatrix4x4;
    determinant:                                 Single;
    polygonCount, polygonToDrawCount, i, offset: NativeUInt;
begin
    if (not Assigned(pAABBTree)) then
        Exit;

    // calculate client rect in OpenGL coordinates
    rect := TQRRect.Create(-1.0, 1.0, 2.0, 2.0);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    rayPos := pRenderer.MousePosToGLPoint(Handle, rect);
    rayDir := TQRVector3D.Create(0.0, 0.0, 1.0);

    // this is a lazy way to correct a perspective issue. In fact, the real model size differs from
    // its image on the screen, but it is placed in a such manner that it seems perfectly framed on
    // the screen. In the model coordinates, the ray location is beyond the mouse coordinate. To
    // correct that, a ratio is needed to
    // keep the ray coordinates coherent with the mouse position. Not ideal (e.g. the model feet are
    // not always well detected), but this is efficient for the majority of cases
    rayPos.MulAndAssign(0.19);

    invertMatrix := modelMatrix.Inverse(determinant);
    rayPos       := invertMatrix.Transform(rayPos);
    rayDir       := invertMatrix.Transform(rayDir);

    // create and populate ray from mouse position
    pRay     := TQRRay.Create;
    pRay.Pos := @rayPos;
    pRay.Dir := @rayDir;

    // get polygons to check for collision by resolving AABB tree
    pAABBTree.Resolve(pRay, polygons);

    polygonCount := Length(polygons);

    // iterate through polygons to check
    if (polygonCount > 0) then
        for i := 0 to polygonCount - 1 do
            // is polygon intersecting ray?
            if (TQRCollisionHelper.GetRayPolygonCollision(pRay, polygons[i])) then
            begin
                // ad polygon in collision to resulting list
                SetLength(polygonToDraw, Length(polygonToDraw) + 1);
                polygonToDraw[Length(polygonToDraw) - 1] := polygons[i];
            end;

    polygonToDrawCount := Length(polygonToDraw);

    // found polgons to draw?
    if (polygonToDrawCount = 0) then
        Exit;

    try
        SetLength(mesh, 1);

        //TQRVertex vertex;
        mesh[0].m_Type      := EQR_VT_Triangles;
        mesh[0].m_CoordType := EQR_VC_XYZ;
        mesh[0].m_Stride    := 7;
        Include(mesh[0].m_Format, EQR_VF_Colors);
        SetLength(mesh[0].m_Buffer, polygonToDrawCount * (mesh[0].m_Stride * 3));

        offset := 0;

        // iterate through polygons to draw
        for i := 0 to polygonToDrawCount - 1 do
        begin
            // build polygon to show
            mesh[0].m_Buffer[offset]      := polygonToDraw[i].Vertex1.X;
            mesh[0].m_Buffer[offset + 1]  := polygonToDraw[i].Vertex1.Y;
            mesh[0].m_Buffer[offset + 2]  := polygonToDraw[i].Vertex1.Z;
            mesh[0].m_Buffer[offset + 3]  := 1.0;
            mesh[0].m_Buffer[offset + 4]  := 0.0;
            mesh[0].m_Buffer[offset + 5]  := 0.0;
            mesh[0].m_Buffer[offset + 6]  := 1.0;
            mesh[0].m_Buffer[offset + 7]  := polygonToDraw[i].Vertex2.X;
            mesh[0].m_Buffer[offset + 8]  := polygonToDraw[i].Vertex2.Y;
            mesh[0].m_Buffer[offset + 9]  := polygonToDraw[i].Vertex2.Z;
            mesh[0].m_Buffer[offset + 10] := 0.8;
            mesh[0].m_Buffer[offset + 11] := 0.0;
            mesh[0].m_Buffer[offset + 12] := 0.2;
            mesh[0].m_Buffer[offset + 13] := 1.0;
            mesh[0].m_Buffer[offset + 14] := polygonToDraw[i].Vertex3.X;
            mesh[0].m_Buffer[offset + 15] := polygonToDraw[i].Vertex3.Y;
            mesh[0].m_Buffer[offset + 16] := polygonToDraw[i].Vertex3.Z;
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
end;

end.
