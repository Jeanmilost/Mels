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
 @abstract(@name is a 3D ray picking with AABB simplification demo.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit Main;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     System.Variants,
     System.Actions,
     System.UITypes,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.StdCtrls,
     Vcl.ExtCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Vcl.ActnList,
     Vcl.Menus,
     Winapi.Messages,
     Winapi.Windows,
     Winapi.OpenGL,
     Winapi.OpenGLext,
     UTQRSmartPointer,
     UTQRGraphics,
     UTQR3D,
     UTQRGeometry,
     UTQRCollision,
     UTQRModel,
     UTQRModelGroup,
     UTQRShapes,
     UTQRShapeGroup,
     UTQROpenGLHelper;

type
    {**
     3D ray picking with AABB simplification demo main form
    }
    TMainForm = class(TForm)
        published
            paInfo: TPanel;
            btRotate: TButton;
            paCollisionResult: TPanel;
            laInCollision: TLabel;
            laTotal: TLabel;
            laToTest: TLabel;
            laHighestHit: TLabel;
            pmOptions: TPopupMenu;
            miRotateSphere: TMenuItem;
            alRotate: TActionList;
            acRotate: TAction;

            procedure FormShow(pSender: TObject);
            procedure FormPaint(pSender: TObject);
            procedure acRotateExecute(pSender: TObject);

        private
            m_hDC:                  THandle;
            m_hRC:                  THandle;
            m_pSphere:              TQRSphereGroup;
            m_Mesh:                 TQRMesh;
            m_pAABBTree:            TQRAABBTree;
            m_PreviousTime:         NativeUInt;
            m_CollidePolygonsCount: NativeUInt;
            m_HighestHit:           NativeUInt;
            m_Theta:                Single;
            m_Rotate:               Boolean;

            {**
             Shows collision detection status
             @param(toTest Polygons to test count)
             @param(inCollision Polygons in collision count)
            }
            procedure ShowStatus(toTest, inCollision: Integer);

            {**
             Configures OpenGL
            }
            procedure ConfigOpenGL;

            {**
             Called when static model will be drawn and caller should extract mesh
             @param(pGroup Group at which model belongs)
             @param(pModel Model to draw)
             @param(textures Texture list, in order they should be linked)
             @param(matrix Model matrix)
            }
            procedure OnDrawCustomStaticModelItem(const pGroup: TQRModelGroup;
                                                        pModel: TQRModel;
                                                const textures: TQRTextures;
                                                  const matrix: TQRMatrix4x4);

        protected
            {**
             Prepares and draws OpenGL scene
            }
            procedure RenderGLScene; virtual;

            {**
             Draws scene
             @param(elapsedTime Elapsed time since last draw)
            }
            procedure DrawScene(const elapsedTime: Double); virtual;

            {**
             Called when application do nothing
             @param(pSender Event sender)
             @param(done @bold([in, out]) If @true, loop will be considered as completed)
            }
            procedure OnIdle(pSender: TObject; var done: Boolean); virtual;

        public
            {**
             Constructor
             @param(pOwner Form owner)
            }
            constructor Create(pOwner: TComponent); override;

            {**
             Destructor
            }
            destructor Destroy; override;
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

    m_hDC                  := 0;
    m_hRC                  := 0;
    m_pSphere              := nil;
    m_pAABBTree            := nil;
    m_PreviousTime         := 0;
    m_CollidePolygonsCount := 0;
    m_HighestHit           := 0;
    m_Theta                := 0.0;
    m_Rotate               := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.Destroy;
begin
    // delete AABB tree
    m_pAABBTree.Free;

    // delete sphere model
    m_pSphere.Free;

    // shutdown OpenGL
    TQROpenGLHelper.DisableOpenGL(Handle, m_hDC, m_hRC);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormShow(pSender: TObject);
var
    pColor: IQRSmartPointer<TQRColor>;
begin
    // was OpenGL already initialized?
    if (m_hRC <> 0) then
        Exit;

    // initialize OpenGL
    if (not TQROpenGLHelper.EnableOpenGL(Handle, m_hDC, m_hRC)) then
    begin
        MessageDlg('OpenGL could not be initialized.' + #13#10#13#10 + 'Application will close.',
                   mtError,
                   [mbOK],
                   0);

        Application.Terminate;
        Exit;
    end;

    // configure OpenGL
    ConfigOpenGL;
    TQROpenGLHelper.CreateViewport(ClientWidth, ClientHeight, False);

    // create and configure sphere
    m_pSphere                  := TQRSphereGroup.Create;
    m_pSphere.OnCustomDrawItem := OnDrawCustomStaticModelItem;

    pColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(0, 0, 255, 255));

    m_pSphere.Load(20,
                   20,
                   1.0,
                   pColor,
                   [EQR_MO_Without_Normals, EQR_MO_Without_Textures]);

    // from now, OpenGL will draw scene every time the thread do nothing else
    Application.OnIdle := OnIdle;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormPaint(pSender: TObject);
begin
    RenderGLScene;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.acRotateExecute(pSender: TObject);
begin
    acRotate.Checked := not acRotate.Checked;
    m_Rotate         := acRotate.Checked;

    if (m_Rotate) then
        btRotate.Caption := 'Stop'
    else
        btRotate.Caption := 'Rotate';
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ShowStatus(toTest, inCollision: Integer);
begin
    // show collision detection status
    laTotal.Caption       := 'Total: '        + IntToStr(m_CollidePolygonsCount);
    laToTest.Caption      := 'To test: '      + IntToStr(toTest);
    laInCollision.Caption := 'In collision: ' + IntToStr(inCollision);
    laHighestHit.Caption  := 'Highest hit: '  + IntToStr(m_HighestHit);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ConfigOpenGL;
begin
    // configure OpenGL depth testing
    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_TRUE);
    glDepthFunc(GL_LEQUAL);
    glDepthRange(0.0, 1.0);

    // enable culling
    glDisable(GL_CULL_FACE);
    glCullFace(GL_NONE);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnDrawCustomStaticModelItem(const pGroup: TQRModelGroup;
                                                      pModel: TQRModel;
                                              const textures: TQRTextures;
                                                const matrix: TQRMatrix4x4);
var
    pSphereModel:                        TQRSphereModel;
    rect:                                TQRRect;
    rayPos, rayDir:                      TQRVector3D;
    rotateMatrix:                        TQRMatrix4x4;
    polygons, polygonToDraw:             TQRPolygons;
    polygonCount, polygonToDrawCount, i: NativeUInt;
    pRay:                                IQRSmartPointer<TQRRay>;
begin
    // no model?
    if (not Assigned(pModel)) then
        Exit;

    // model mesh still not created?
    if ((Length(m_Mesh) = 0) and (not Assigned(m_pAABBTree))) then
    begin
        // get sphere model
        pSphereModel := TQRSphereModel(pModel);

        // found it?
        if (not Assigned(pSphereModel)) then
            Exit;

        // create new aligned-axis bouding box tree
        m_pAABBTree := TQRAABBTree.Create;

        // get sphere mesh to draw and aligned-axis bounding box tree to use
        pSphereModel.GetMesh(m_Mesh, m_pAABBTree);
    end;

    // draw mesh
    TQROpenGLHelper.Draw(m_Mesh, matrix, textures);

    rect := TQRRect.Create(-1.0, 1.0, 2.0, 2.0);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    rayPos := TQROpenGLHelper.MousePosToGLPoint(Handle, rect);
    rayDir := TQRVector3D.Create(0.0, 0.0, 1.0);

    // prepare rotation matrix
    rotateMatrix := TQRMatrix4x4.Identity;
    rotateMatrix.Rotate(-m_Theta, TQRVector3D.Create(0.0, 1.0, 0.0));

    // rotate ray position and direction
    rayPos := rotateMatrix.Transform(rayPos);
    rayDir := rotateMatrix.Transform(rayDir);

    // create and populate ray from mouse position
    pRay     := TQRSmartPointer<TQRRay>.Create();
    pRay.Pos := @rayPos;
    pRay.Dir := @rayDir;

    // get polygons to check for collision by resolving AABB tree
    m_pAABBTree.Resolve(pRay, polygons);

    polygonCount := Length(polygons);

    // update highest hit
    m_HighestHit := Max(m_HighestHit, polygonCount);

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

    glPushMatrix;

    // place triangles into 3D world
    glLoadMatrixf(PGLfloat(matrix.GetPtr));

    // found collide polygons to draw?
    if (polygonToDrawCount > 0) then
        for i := 0 to polygonToDrawCount - 1 do
        begin
            glBegin(GL_TRIANGLES);

            // draw vertex 1
            glColor3f(1.0, 0.0, 0.0);
            glVertex3f(polygonToDraw[i].Vertex1.X,
                       polygonToDraw[i].Vertex1.Y,
                       polygonToDraw[i].Vertex1.Z);

            // draw vertex 2
            glColor3f(0.8, 0.0, 0.2);
            glVertex3f(polygonToDraw[i].Vertex2.X,
                       polygonToDraw[i].Vertex2.Y,
                       polygonToDraw[i].Vertex2.Z);

            // draw vertex 3
            glColor3f(1.0, 0.12, 0.2);
            glVertex3f(polygonToDraw[i].Vertex3.X,
                       polygonToDraw[i].Vertex3.Y,
                       polygonToDraw[i].Vertex3.Z);

            glEnd;

            glFlush;
        end;

    glPopMatrix;

    // show collision detection status
    ShowStatus(polygonCount, polygonToDrawCount);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.RenderGLScene;
var
    now:         NativeUInt;
    elapsedTime: Double;
begin
    // calculate time interval
    now            := GetTickCount;
    elapsedTime    := (now - m_PreviousTime);
    m_PreviousTime :=  now;

    // clear scene
    glClearColor(0.0, 0.0, 0.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // draw scene
    DrawScene(elapsedTime);

    glFlush;

    // finalize scene
    SwapBuffers(m_hDC);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.DrawScene(const elapsedTime: Double);
var
    fullAngle: Single;
begin
    // do rotate sphere?
    if (m_Rotate) then
    begin
        // calculate full max angle (i.e. 360°)
        fullAngle := PI * 2.0;

        // calculate next rotation angle
        if (m_Theta + 0.001 > fullAngle) then
            m_Theta := (m_Theta + 0.001) - fullAngle
        else
            m_Theta := m_Theta + 0.001;
    end;

    m_pSphere.RotationY := m_Theta;

    m_pSphere.Draw(elapsedTime);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnIdle(pSender: TObject; var done: Boolean);
begin
    done := False;

    // draw OpenGL scene every time application do nothing
    RenderGLScene;
end;
//--------------------------------------------------------------------------------------------------

end.
