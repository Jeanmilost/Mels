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
 @abstract(@name contains the 3D ray picking with AABB tree simplification demo main form.)
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
     Vcl.ActnList,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.StdCtrls,
     Vcl.ExtCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Vcl.Menus,
     Winapi.Messages,
     Winapi.Windows,
     Winapi.OpenGL,
     UTQRSmartPointer,
     UTQRHelpers,
     UTQRGraphics,
     UTQR3D,
     UTQRGeometry,
     UTQRCollision,
     UTQRShapes,
     UTQROpenGLHelper;

type
    {**
     Main form class
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

            procedure FormPaint(pSender: TObject);
            procedure FormShow(pSender: TObject);
            procedure acRotateExecute(pSender: TObject);

        private
            m_hDC:                  THandle;
            m_hRC:                  THandle;
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
            procedure ShowStatus(toTest, inCollision: NativeInt);

            {**
             Configures OpenGL
            }
            procedure ConfigOpenGL;

        protected
            {**
             Prepares and draws OpenGL scene
            }
            procedure RenderGLScene; virtual;

            {**
             Draws scene
             @param(elapsedTime Elapsed time since last draw)
            }
            procedure DrawScene(const elapsedTime: Double);

            {**
             Called when application do nothing
             @param pSender - event sender
             @param[in, out] done - if true, loop will be considered as completed
            }
            procedure OnIdle(pSender: TObject; var done: Boolean);

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
var
    pColor:       IQRSmartPointer<TQRColor>;
    pSphereModel: IQRSmartPointer<TQRSphereModel>;
begin
    inherited Create(pOwner);

    m_hDC                  := 0;
    m_hRC                  := 0;
    m_pAABBTree            := nil;
    m_PreviousTime         := 0;
    m_CollidePolygonsCount := 0;
    m_HighestHit           := 0;
    m_Theta                := 0.0;
    m_Rotate               := False;

    pColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(0, 0, 255, 255));

    // create a demo blue sphere
    pSphereModel              := TQRSmartPointer<TQRSphereModel>.Create();
    pSphereModel.Slices       := 20;
    pSphereModel.Stacks       := 20;
    pSphereModel.Radius       := 1.0;
    pSphereModel.Color        := pColor;
    pSphereModel.VertexFormat := [EQR_VF_Colors];

    // create aligned-axis bounding box tree
    m_pAABBTree := TQRAABBTree.Create;

    pSphereModel.GetMesh(m_Mesh, m_pAABBTree);
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.Destroy;
begin
    // delete aabb tree
    m_pAABBTree.Free;

    // shutdown OpenGL
    TQROpenGLHelper.DisableOpenGL(Handle, m_hDC, m_hRC);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormShow(pSender: TObject);
begin
    // was OpenGL already initialized?
    if (m_hRC <> 0) then
        Exit;

    // initialize OpenGL
    if (not TQROpenGLHelper.EnableOpenGL(Handle, m_hDC, m_hRC)) then
    begin
        MessageDlg('OpenGL could not be initialized.\r\n\r\nApplication will close.',
                   mtError,
                   [mbOK],
                   0);

        Application.Terminate;
        Exit;
    end;

    // configure OpenGL
    ConfigOpenGL;
    TQROpenGLHelper.CreateViewport(ClientWidth, ClientHeight, false);

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
    m_Rotate         :=     acRotate.Checked;

    if (m_Rotate) then
        btRotate.Caption := 'Stop'
    else
        btRotate.Caption := 'Rotate';
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ShowStatus(toTest, inCollision: NativeInt);
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
    textures:                            TQRTextures;
    rect:                                TQRRect;
    rayPos, rayDir:                      TQRVector3D;
    rotateMatrix:                        TQRMatrix4x4;
    pRay:                                IQRSmartPointer<TQRRay>;
    polygons, polygonToDraw:             TQRPolygons;
    polygonCount, polygonToDrawCount, i: NativeUInt;
begin
    // do rotate sphere?
    if (m_Rotate) then
    begin
        // calculate next rotation angle
        m_Theta := m_Theta + (0.001 * elapsedTime);

        // correct it if out of bounds
        if (m_Theta > (PI * 2.0)) then
            m_Theta := (m_Theta - (PI * 2.0));
    end;

    // draw mesh
    TQROpenGLHelper.Draw(m_Mesh,
                         TQRVector3D.Create(0.0, 0.0, 0.0),
                         0.0,
                         m_Theta,
                         0.0,
                         TQRVector3D.Create(1.0, 1.0, 1.0),
                         textures);

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
    glTranslatef(0.0, 0.0, 0.0);
    glRotatef(0.0, 1.0, 0.0, 0.0);
    glRotatef(TQRMathsHelper.RadToDeg(m_Theta), 0.0, 1.0, 0.0);
    glRotatef(0.0, 0.0, 0.0, 1.0);
    glScalef(1.0, 1.0, 1.0);

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
procedure TMainForm.OnIdle(pSender: TObject; var Done: Boolean);
begin
    done := False;

    // draw OpenGL scene every time application do nothing
    RenderGLScene;
end;
//--------------------------------------------------------------------------------------------------

end.
