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
 @abstract(@name contains the MD3 demo main form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit Main;

interface

uses System.Classes,
     System.UITypes,
     System.SysUtils,
     System.Math,
     System.Variants,
     System.Generics.Collections,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.ExtCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Winapi.Messages,
     Winapi.Windows,
     Winapi.OpenGL,
     Winapi.OpenGLext,
     UTQRSmartPointer,
     UTQR3D,
     UTQRGraphics,
     UTQRGeometry,
     UTQRCollision,
     UTQRModel,
     UTQRMD3,
     UTQRShaderOpenGL,
     UTQROpenGLHelper,
     UTOptions;

type
    {**
     MD3 demo main form
    }
    TMainForm = class(TForm)
        published
            paRendering: TPanel;

            procedure FormCreate(pSender: TObject);
            procedure FormResize(pSender: TObject);
            procedure FormKeyPress(pSender: TObject; var Key: Char);
            procedure FormPaint(pSender: TObject);

        private type
            {**
             Frame structure, contains the local model cache
            }
            IFrame = class
                private
                    m_pMesh:     TQRMesh;
                    m_pAABBTree: TQRAABBTree;

                public
                    {**
                     Constructor
                     @param(useCollisions - if @true, collisions detection is enabled)
                    }
                    constructor Create(useCollisions: Boolean); virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            IFrames = TObjectDictionary<NativeUInt, IFrame>;

            {**
             Item associating a texture identifier with a model name
            }
            ITextureItem = class
                private
                    m_ID:   NativeUInt;
                    m_Name: UnicodeString;

                public
                    {**
                     Constructor
                    }
                    constructor Create; overload; virtual;

                    {**
                     Constructor
                     @param(id Texture identifier)
                     @param(name Texture name to link with, as defined in model)
                    }
                    constructor Create(id: NativeUInt; const name: UnicodeString); overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            ITextureTable = TList<ITextureItem>;

        private
            m_pFrames:                  IFrames;
            m_hDC:                      THandle;
            m_hRC:                      THandle;
            m_pMD3:                     TQRMD3Model;
            m_pColorShader:             TQRShaderOpenGL;
            m_pTextureShader:           TQRShaderOpenGL;
            m_Textures:                 TQRTextures;
            m_ProjectionMatrix:         TQRMatrix4x4;
            m_ViewMatrix:               TQRMatrix4x4;
            m_ModelMatrix:              TQRMatrix4x4;
            m_InterpolationFactor:      Double;
            m_PreviousTime:             NativeUInt;
            m_FrameIndex:               NativeUInt;
            m_FPS:                      NativeUInt;
            m_FullScreen:               Boolean;
            m_UseShader:                Boolean;
            m_UsePreCalculatedLighting: Boolean;
            m_Collisions:               Boolean;

            {**
             Configures OpenGL
            }
            procedure ConfigOpenGL;

            {**
             Builds shader
             @param(pVertexPrg Stream containing vertex program)
             @param(pFragmentPrg Stream containing fragment program)
             @param(pShader Shader to populate)
             @return(@true on success, otherwise @false)
            }
            function BuildShader(pVertexPrg, pFragmentPrg: TStream;
                                                  pShader: TQRShaderOpenGL): Boolean;

            {**
             Loads MD3 model
             @param(useShader If @true, shader will be used)
             @return(@true on success, otherwise @false)
            }
            function LoadModel(useShader: Boolean): Boolean;

            {**
             Gets frame from local cache
             @param(index Frame index)
             @param(pModel Model for which next frame should be get)
             @param(useCollision If @true, collisions are used)
             @return(Frame)
            }
            function GetFrame(index: NativeUInt; pModel: TQRMD3Model; useCollision: Boolean): IFrame;

            {**
             Detects collision with mouse pointer and draws the polygons in collision
             @param(modelMatrix Model matrix)
             @param(pAABBTree Aligned-axis bounding box tree)
             @param(useShader Whether or not shader are used to draw model)
             @param(collisions Whether or not collisions are visible)
            }
            procedure DetectAndDrawCollisions(const modelMatrix: TQRMatrix4x4;
                                                const pAABBTree: TQRAABBTree;
                                          useShader, collisions: Boolean);

            {**
             Prepres the shader to draw the model
             @param(pShader Shader to prepare)
             @param(textures Textures belonging to model)
            }
            procedure PrepareShaderToDrawModel(pShader: TQRShaderOpenGL; const textures: TQRTextures);

            {**
             Loads model texture
             @param(textureTable Texture table)
             @return(@true on success, otherwise @false)
            }
            function LoadTexture(const textureTable: ITextureTable): Boolean;

            {**
             Draws model
             @param(pGroup Group at which model belongs)
             @param(pModel Model to draw)
             @param(textures Textures belonging to model, in the order where they should be combined)
             @param(matrix Model matrix)
             @param(index Model mesh index$9
             @param(nextIndex Model mesh index to interpolate with)
             @param(interpolationFactor Interpolation factor)
             @param(useShader Whether or not shader are used to draw model)
             @param(collisions Whether or not collisions are visible)
            }
            procedure DrawModel(pModel: TQRMD3Model;
                        const textures: TQRTextures;
                          const matrix: TQRMatrix4x4;
                      index, nextIndex: NativeInt;
                   interpolationFactor: Double;
                 useShader, collisions: Boolean);

        protected
            {**
             Called when application do nothing
             @param(pSender Event sender)
             @param(done @bold([in, out]) If @true, loop will be considered as completed)
            }
            procedure OnIdle(pSender: TObject; var done: Boolean); virtual;

            {**
             Renders (i.e. prepares and draws) scene
            }
            procedure RenderGLScene; virtual;

            {**
             Draws scene
             @param(elapsedTime Elapsed time since last draw)
            }
            procedure Draw(const elapsedTime: Double); virtual;

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
// TMainForm.IFrame
//--------------------------------------------------------------------------------------------------
constructor TMainForm.IFrame.Create(useCollisions: Boolean);
begin
    inherited Create;

    if (useCollisions) then
        m_pAABBTree := TQRAABBTree.Create
    else
        m_pAABBTree := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.IFrame.Destroy;
begin
    m_pAABBTree.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TMainForm.ITextureItem
//--------------------------------------------------------------------------------------------------
constructor TMainForm.ITextureItem.Create;
begin
    inherited Create;

    m_ID := 0;
end;
//--------------------------------------------------------------------------------------------------
constructor TMainForm.ITextureItem.Create(id: NativeUInt; const name: UnicodeString);
begin
    inherited Create;

    m_ID   := id;
    m_Name := name;
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.ITextureItem.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_pFrames                  := IFrames.Create([doOwnsValues]);
    m_hDC                      := 0;
    m_hRC                      := 0;
    m_pMD3                     := nil;
    m_pColorShader             := nil;
    m_pTextureShader           := nil;
    m_InterpolationFactor      := 0.0;
    m_PreviousTime             := GetTickCount;
    m_FrameIndex               := 0;
    m_FPS                      := 12;
    m_FullScreen               := False;
    m_UseShader                := True;
    m_UsePreCalculatedLighting := True;
    m_Collisions               := True;
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.Destroy;
var
    pTexture: TQRTexture;
begin
    // delete textures
    for pTexture in m_Textures do
        pTexture.Free;

    // delete cached frames, if any
    m_pFrames.Free;

    // delete color shader
    m_pColorShader.Free;

    // delete texture shader
    m_pTextureShader.Free;

    // delete MD3 model
    m_pMD3.Free;

    // shutdown OpenGL
    TQROpenGLHelper.DisableOpenGL(Handle, m_hDC, m_hRC);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormCreate(pSender: TObject);
var
    pOptions: IQRSmartPointer<TOptions>;
begin
    // show options
    pOptions                      := TQRSmartPointer<TOptions>.Create(TOptions.Create(Self));
    pOptions.ckFullScreen.Checked := m_FullScreen;
    pOptions.ckUseShader.Checked  := m_UseShader;
    pOptions.ckCollisions.Checked := m_Collisions;
    pOptions.ShowModal();

    // apply options
    m_FullScreen := pOptions.ckFullScreen.Checked;
    m_UseShader  := pOptions.ckUseShader.Checked;
    m_Collisions := pOptions.ckCollisions.Checked;

    // do show model in full screen?
    if (m_FullScreen) then
    begin
        BorderStyle := bsNone;
        WindowState := wsMaximized;
        ShowCursor(m_Collisions);
    end
    else
    begin
        BorderStyle := bsSizeable;
        WindowState := wsNormal;
        ShowCursor(True);
    end;

    BringToFront;

    // select correct cursor to use
    if (m_Collisions) then
        paRendering.Cursor := crCross
    else
        paRendering.Cursor := crDefault;

    // initialize OpenGL
    if (not TQROpenGLHelper.EnableOpenGL(paRendering.Handle, m_hDC, m_hRC)) then
    begin
        MessageDlg('OpenGL could not be initialized.' + #13#10#13#10 + 'Application will close.',
                   mtError,
                   [mbOK],
                   0);

        Application.Terminate;
        Exit;
    end;

    // do use shader?
    if (m_UseShader) then
        InitOpenGLext;

    // configure OpenGL
    ConfigOpenGL;

    // load MD3 model
    if (not LoadModel(m_UseShader)) then
    begin
        MessageDlg('Failed to load MD3 model.' + #13#10#13#10 + 'Application will close.',
                   mtError,
                   [mbOK],
                   0);

        Application.Terminate;
        Exit;
    end;

    // from now, OpenGL will draw scene every time the thread do nothing else
    Application.OnIdle := OnIdle;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormResize(pSender: TObject);
var
    fov, zNear, zFar, widthF, heightF, aspectRatio, maxX, maxY: GLfloat;
    position, direction, up:                                    TQRVector3D;
begin
    fov   := 45.0;
    zNear := 0.1;
    zFar  := 100.0;

    widthF  := ClientWidth;
    heightF := ClientHeight;

    // is width out of bounds?
    if (widthF = 0.0) then
        widthF := 1.0;

    // is height out of bounds?
    if (heightF = 0.0) then
        heightF := 1.0;

    // calculate aspect ratio
    aspectRatio := widthF / heightF;

    // create projection matrix (will not be modified while execution)
    m_ProjectionMatrix := TQROpenGLHelper.GetPerspective(fov,
                                                         aspectRatio,
                                                         zNear,
                                                         zFar,
                                                         True);

    position  := Default(TQRVector3D);
    direction := TQRVector3D.Create(0.0, 0.0, 1.0);
    up        := TQRVector3D.Create(0.0, 1.0, 0.0);

    // create view matrix (will not be modified while execution)
    m_ViewMatrix := TQROpenGLHelper.LookAtLH(position, direction, up);

    // create viewport
    TQROpenGLHelper.CreateViewport(ClientWidth, ClientHeight, not m_UseShader);

    // configure matrices for OpenGL direct mode
    if (not m_UseShader) then
    begin
        maxY := zNear * Tan(fov * PI / 360.0);
        maxX := maxY  * aspectRatio;

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glOrtho(-maxX, maxX, -maxY, maxY, zNear, zFar);
        glMatrixMode(GL_MODELVIEW);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormKeyPress(pSender: TObject; var key: Char);
begin
    case (Integer(key)) of
        VK_ESCAPE: Application.Terminate;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormPaint(pSender: TObject);
begin
    RenderGLScene;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ConfigOpenGL;
begin
    // configure OpenGL
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);
    glEnable(GL_TEXTURE_2D);
end;
//--------------------------------------------------------------------------------------------------
function TMainForm.BuildShader(pVertexPrg, pFragmentPrg: TStream;
                                                pShader: TQRShaderOpenGL): Boolean;
begin
    // load and compile shader
    pShader.CreateProgram;
    pShader.AttachFile(pVertexPrg,   EQR_ST_Vertex);
    pShader.AttachFile(pFragmentPrg, EQR_ST_Fragment);

    // try to link shader
    Result := pShader.Link(False);
end;
//--------------------------------------------------------------------------------------------------
function TMainForm.LoadModel(useShader: Boolean): Boolean;
var
    hPackageInstance:                       THandle;
    pVertexPrg, pFragmentPrg, pModelStream: TResourceStream;
    pColor:                                 TQRColor;
    textureTable:                           ITextureTable;
begin
    // delete cached frames, if any
    m_pFrames.Clear;

    // get module instance at which this form belongs
    hPackageInstance := FindClassHInstance(TMainForm);

    // found it?
    if (hPackageInstance = 0) then
        Exit(False);

    // do use shader?
    if (useShader) then
    begin
        pVertexPrg   := nil;
        pFragmentPrg := nil;

        // color shader still not loaded?
        if (not Assigned(m_pColorShader)) then
            try
                // found resource containing the vertex shader program to load?
                if (FindResource(hPackageInstance, PChar('ID_COLOR_VERTEX_SHADER'), RT_RCDATA) <> 0)
                then
                    pVertexPrg := TResourceStream.Create(hPackageInstance,
                                                         PChar('ID_COLOR_VERTEX_SHADER'),
                                                         RT_RCDATA);

                if (not Assigned(pVertexPrg)) then
                    Exit(False);

                // found resource containing the fragment shader program to load?
                if (FindResource(hPackageInstance, PChar('ID_COLOR_FRAGMENT_SHADER'), RT_RCDATA) <> 0)
                then
                    pFragmentPrg := TResourceStream.Create(hPackageInstance,
                                                           PChar('ID_COLOR_FRAGMENT_SHADER'),
                                                           RT_RCDATA);

                if (not Assigned(pFragmentPrg)) then
                    Exit(False);

                // create color shader
                m_pColorShader := TQRShaderOpenGL.Create;

                // try to build shader
                if (not BuildShader(pVertexPrg, pFragmentPrg, m_pColorShader)) then
                    Exit(False);
            finally
                // delete resource streams, if needed
                pVertexPrg.Free;
                pFragmentPrg.Free;
            end;

        pVertexPrg   := nil;
        pFragmentPrg := nil;

        // texture shader still not loaded?
        if (not Assigned(m_pTextureShader)) then
            try
                // found resource containing the vertex shader program to load?
                if (FindResource(hPackageInstance, PChar('ID_TEXTURE_VERTEX_SHADER'), RT_RCDATA) <> 0)
                then
                    pVertexPrg := TResourceStream.Create(hPackageInstance,
                                                         PChar('ID_TEXTURE_VERTEX_SHADER'),
                                                         RT_RCDATA);

                if (not Assigned(pVertexPrg)) then
                    Exit(False);

                // found resource containing the fragment shader program to load?
                if (FindResource(hPackageInstance, PChar('ID_TEXTURE_FRAGMENT_SHADER'), RT_RCDATA) <> 0)
                then
                    pFragmentPrg := TResourceStream.Create(hPackageInstance,
                                                           PChar('ID_TEXTURE_FRAGMENT_SHADER'),
                                                           RT_RCDATA);

                if (not Assigned(pFragmentPrg)) then
                    Exit(False);

                // create texture shader
                m_pTextureShader := TQRShaderOpenGL.Create;

                // try to build shader
                if (not BuildShader(pVertexPrg, pFragmentPrg, m_pTextureShader)) then
                    Exit(False);
            finally
                // delete resource streams, if needed
                pVertexPrg.Free;
                pFragmentPrg.Free;
            end;
    end;

    // create MD3 model, if needed
    if (not Assigned(m_pMD3)) then
        m_pMD3 := TQRMD3Model.Create;

    pColor       := nil;
    pModelStream := nil;
    textureTable := nil;

    try
        // load MD3 model from resources
        if (FindResource(hPackageInstance, PChar('ID_MD3_MODEL'), RT_RCDATA) <> 0)
        then
            pModelStream := TResourceStream.Create(hPackageInstance,
                                                   PChar('ID_MD3_MODEL'),
                                                   RT_RCDATA);

        if (not Assigned(pModelStream)) then
            Exit(False);

        pColor := TQRColor.Create(255, 255, 255, 255);

        // load model
        if (not m_pMD3.Load(pModelStream, pModelStream.Size)) then
            Exit(False);

        m_pMD3.VertexFormat := [EQR_VF_TexCoords, EQR_VF_Colors];

        // create model matrix
        m_ModelMatrix := TQRMatrix4x4.Identity;
        m_ModelMatrix.Translate(TQRVector3D.Create(0.0, -0.03, -1.5));
        m_ModelMatrix.Rotate(-(PI / 4), TQRVector3D.Create(1.0, 0.0, 0.0)); // -45°
        m_ModelMatrix.Rotate(-(PI / 4), TQRVector3D.Create(0.0, 0.0, 1.0)); // -45°
        m_ModelMatrix.Scale(TQRVector3D.Create(0.001, 0.001, 0.001));

        textureTable := ITextureTable.Create;

        // link texture to use with model parts
        textureTable.Add(ITextureItem.Create(3, 'lower_glass'));
        textureTable.Add(ITextureItem.Create(1, 'clock_face'));
        textureTable.Add(ITextureItem.Create(3, 'clock_glass'));
        textureTable.Add(ITextureItem.Create(2, 'feet'));
        textureTable.Add(ITextureItem.Create(2, 'pendulam'));
        textureTable.Add(ITextureItem.Create(4, 'small_hand'));
        textureTable.Add(ITextureItem.Create(0, 'big_hand'));
        textureTable.Add(ITextureItem.Create(2, 'balance'));
        textureTable.Add(ITextureItem.Create(2, 'clock_body'));
        textureTable.Add(ITextureItem.Create(2, 'clock_face_top'));

        // load all textures
        LoadTexture(textureTable);
    finally
        textureTable.Free;
        pModelStream.Free;
        pColor.Free;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TMainForm.GetFrame(index: NativeUInt; pModel: TQRMD3Model; useCollision: Boolean): IFrame;
var
    frameAdded: Boolean;
begin
    // get frame from cache
    if (m_pFrames.TryGetValue(index, Result)) then
        Exit;

    frameAdded := False;
    Result     := IFrame.Create(useCollision);

    try
        // frame still not cached, create and add it
        pModel.GetMesh(index, Result.m_pMesh, Result.m_pAABBTree);
        m_pFrames.Add(index, Result);
        frameAdded := True;
    finally
        // delete newly created frame only if an error occurred
        if (not frameAdded) then
        begin
            Result.Free;
            Result := nil;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.DetectAndDrawCollisions(const modelMatrix: TQRMatrix4x4;
                                              const pAABBTree: TQRAABBTree;
                                        useShader, collisions: Boolean);
var
    rect:                       TQRRect;
    rayPos, rayDir:             TQRVector3D;
    invertModel:                TQRMatrix4x4;
    determinant:                Single;
    pRay:                       TQRRay;
    mesh:                       TQRMesh;
    polygons, polygonToDraw:    TQRPolygons;
    polygon:                    TQRPolygon;
    textures:                   TQRTextures;
    polygonToDrawCount, offset: NativeUInt;
    uniform:                    GLint;
begin
    if ((not collisions) or (not Assigned(pAABBTree))) then
        Exit;

    // calculate client rect in OpenGL coordinates
    rect := TQRRect.Create(-1.0, 1.0, 2.0, 2.0);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    rayPos := TQROpenGLHelper.MousePosToGLPoint(Handle, rect);
    rayDir := TQRVector3D.Create(0.0, 0.0, 1.0);

    // unproject the ray to make it inside the 3d world coordinates
    TQROpenGLHelper.Unproject(m_ProjectionMatrix, m_ViewMatrix, rayPos, rayDir);

    // now transform the ray to match with the model position
    invertModel := modelMatrix.Inverse(determinant);
    rayPos      := invertModel.Transform(rayPos);
    rayDir      := invertModel.TransformNormal(rayDir);
    rayDir      := rayDir.Normalize();

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

    polygonToDrawCount := Length(polygonToDraw);

    // found polgons to draw?
    if (polygonToDrawCount = 0) then
        Exit;

    SetLength(mesh, 1);

    mesh[0].m_Type      := EQR_VT_Triangles;
    mesh[0].m_CoordType := EQR_VC_XYZ;
    mesh[0].m_Stride    := 7;
    mesh[0].m_Format    := [EQR_VF_Colors];
    SetLength(mesh[0].m_Buffer, polygonToDrawCount * (mesh[0].m_Stride * 3));

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
        Inc(offset, (mesh[0].m_Stride * 3));
    end;

    // do use shader?
    if (useShader) then
    begin
        // bind shader program
        m_pColorShader.Use(true);

        // get perspective (or projection) matrix slot from shader
        uniform := TQROpenGLHelper.GetUniform(m_pColorShader, EQR_SA_PerspectiveMatrix);

        // found it?
        if (uniform = -1) then
            raise Exception.Create('Program uniform not found - perspective');

        // connect perspective (or projection) matrix to shader
        glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(m_ProjectionMatrix.GetPtr));

        // get view (or camera) matrix slot from shader
        uniform := TQROpenGLHelper.GetUniform(m_pColorShader, EQR_SA_CameraMatrix);

        // found it?
        if (uniform = -1) then
            raise Exception.Create('Program uniform not found - camera');

        // connect view (or camera) matrix to shader
        glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(m_ViewMatrix.GetPtr));

        // unbind shader program
        m_pColorShader.Use(false);

        // configure OpenGL to draw polygons in collision
        glDisable(GL_TEXTURE_2D);
        glCullFace(GL_NONE);
        glDisable(GL_DEPTH_TEST);

        // draw mesh
        TQROpenGLHelper.Draw(mesh, modelMatrix, textures, m_pColorShader);

        // restore previous OpenGL parameters
        glEnable(GL_DEPTH_TEST);
        glCullFace(GL_FRONT);
        glEnable(GL_TEXTURE_2D);

        glFlush;
    end
    else
    begin
        glMatrixMode(GL_MODELVIEW);

        glPushMatrix;

        // place triangles into 3D world
        glLoadMatrixf(PGLfloat(modelMatrix.GetPtr));

        // configure OpenGL to draw polygons in collision
        glDisable(GL_TEXTURE_2D);
        glCullFace(GL_NONE);
        glDisable(GL_DEPTH_TEST);

        // draw polygons in collision with mouse pointer
        TQROpenGLHelper.Draw(mesh, modelMatrix, textures);

        // restore previous OpenGL parameters
        glEnable(GL_DEPTH_TEST);
        glCullFace(GL_FRONT);
        glEnable(GL_TEXTURE_2D);

        glPopMatrix;

        glFlush;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.PrepareShaderToDrawModel(pShader: TQRShaderOpenGL; const textures: TQRTextures);
var
    uniform: GLint;
begin
    if (not Assigned(pShader)) then
        Exit;

    // bind shader program
    pShader.Use(True);

    // get perspective (or projection) matrix slot from shader
    uniform := TQROpenGLHelper.GetUniform(pShader, EQR_SA_PerspectiveMatrix);

    // found it?
    if (uniform = -1) then
        raise Exception.Create('Program uniform not found - perspective');

    // connect perspective (or projection) matrix to shader
    glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(m_ProjectionMatrix.GetPtr));

    // get view (or camera) matrix slot from shader
    uniform := TQROpenGLHelper.GetUniform(pShader, EQR_SA_CameraMatrix);

    // found it?
    if (uniform = -1) then
        raise Exception.Create('Program uniform not found - camera');

    // connect view (or camera) matrix to shader
    glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(m_ViewMatrix.GetPtr));

    // unbind shader program
    pShader.Use(False);
end;
//--------------------------------------------------------------------------------------------------
function TMainForm.LoadTexture(const textureTable: ITextureTable): Boolean;
var
    hPackageInstance:       THandle;
    pixelFormat,
    index:                  Integer;
    pTextureBHandStream,
    pTextureClockFaceStream,
    pTextureGFClockStream,
    pTextureGlassPaneStream,
    pTextureSHandStream:    TResourceStream;
    pBitmap:                Vcl.Graphics.TBitmap;
    pPixels:                PByte;
    pTextureItem:           ITextureItem;
begin
    // get module instance at which this form belongs
    hPackageInstance := FindClassHInstance(TMainForm);

    // found it?
    if (hPackageInstance = 0) then
        Exit(False);

    pTextureBHandStream     := nil;
    pTextureClockFaceStream := nil;
    pTextureGFClockStream   := nil;
    pTextureGlassPaneStream := nil;
    pTextureSHandStream     := nil;
    pBitmap                 := nil;
    pPixels                 := nil;

    try
        // load big hand texture image from resources
        if (FindResource(hPackageInstance, PChar('ID_MD3_TEXTURE_BHAND'), RT_RCDATA) <> 0)
        then
            pTextureBHandStream := TResourceStream.Create(hPackageInstance,
                                                          PChar('ID_MD3_TEXTURE_BHAND'),
                                                          RT_RCDATA);

        if (not Assigned(pTextureBHandStream)) then
            Exit(False);

        // load clock face texture image from resources
        if (FindResource(hPackageInstance, PChar('ID_MD3_TEXTURE_CLOCK_FACE'), RT_RCDATA) <> 0)
        then
            pTextureClockFaceStream := TResourceStream.Create(hPackageInstance,
                                                              PChar('ID_MD3_TEXTURE_CLOCK_FACE'),
                                                              RT_RCDATA);

        if (not Assigned(pTextureClockFaceStream)) then
            Exit(False);

        // load clock texture image from resources
        if (FindResource(hPackageInstance, PChar('ID_MD3_TEXTURE_GF_CLOCK'), RT_RCDATA) <> 0)
        then
            pTextureGFClockStream := TResourceStream.Create(hPackageInstance,
                                                            PChar('ID_MD3_TEXTURE_GF_CLOCK'),
                                                            RT_RCDATA);

        if (not Assigned(pTextureGFClockStream)) then
            Exit(False);

        // load glass pane texture image from resources
        if (FindResource(hPackageInstance, PChar('ID_MD3_TEXTURE_GLASS_PANE'), RT_RCDATA) <> 0)
        then
            pTextureGlassPaneStream := TResourceStream.Create(hPackageInstance,
                                                              PChar('ID_MD3_TEXTURE_GLASS_PANE'),
                                                              RT_RCDATA);

        if (not Assigned(pTextureGlassPaneStream)) then
            Exit(False);

        // load small hand texture image from resources
        if (FindResource(hPackageInstance, PChar('ID_MD3_TEXTURE_SHAND'), RT_RCDATA) <> 0)
        then
            pTextureSHandStream := TResourceStream.Create(hPackageInstance,
                                                          PChar('ID_MD3_TEXTURE_SHAND'),
                                                          RT_RCDATA);

        if (not Assigned(pTextureSHandStream)) then
            Exit(False);

        index := 0;

        // iterate through textures to create
        for pTextureItem in textureTable do
        begin
            try
                // reset stream positions
                pTextureBHandStream.Position     := 0;
                pTextureClockFaceStream.Position := 0;
                pTextureGFClockStream.Position   := 0;
                pTextureGlassPaneStream.Position := 0;
                pTextureSHandStream.Position     := 0;

                // assign texture to list, and create and populate new texture
                SetLength(m_Textures, Length(m_Textures) + 1);
                m_Textures[index]      := TQRTexture.Create;
                m_Textures[index].Name := pTextureItem.m_Name;

                // create texture bitmap
                pBitmap := Vcl.Graphics.TBitmap.Create;

                // load MD3 texture
                case (pTextureItem.m_ID) of
                    0: pBitmap.LoadFromStream(pTextureBHandStream);
                    1: pBitmap.LoadFromStream(pTextureClockFaceStream);
                    2: pBitmap.LoadFromStream(pTextureGFClockStream);
                    3: pBitmap.LoadFromStream(pTextureGlassPaneStream);
                    4: pBitmap.LoadFromStream(pTextureSHandStream);
                end;

                if (pBitmap.PixelFormat = pf32bit) then
                    pixelFormat := GL_RGBA
                else
                    pixelFormat := GL_RGB;

                // convert bitmap to pixel array, and create OpenGL texture from array
                TQROpenGLHelper.BytesFromBitmap(pBitmap, pPixels, false, false);
                m_Textures[index].Index := TQROpenGLHelper.CreateTexture(pBitmap.Width,
                                                                         pBitmap.Height,
                                                                         pixelFormat,
                                                                         pPixels,
                                                                         GL_NEAREST,
                                                                         GL_NEAREST,
                                                                         GL_TEXTURE_2D);

                Inc(Index);
            finally
                if (Assigned(pPixels)) then
                    FreeMem(pPixels);

                pBitmap.Free;
            end;
        end;
    finally
        pTextureBHandStream.Free;
        pTextureClockFaceStream.Free;
        pTextureGFClockStream.Free;
        pTextureGlassPaneStream.Free;
        pTextureSHandStream.Free;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.DrawModel(pModel: TQRMD3Model;
                      const textures: TQRTextures;
                        const matrix: TQRMatrix4x4;
                    index, nextIndex: NativeInt;
                 interpolationFactor: Double;
               useShader, collisions: Boolean);
var
    meshCount:                    NativeInt;
    pMeshToDraw, pNextMeshToDraw: PQRMesh;
    pAABBTree:                    TQRAABBTree;
    interpolated:                 Boolean;
    pFrame, pNextFrame:           IFrame;
begin
    // no model to draw?
    if (not Assigned(pModel)) then
        Exit;

    // get mesh count
    meshCount := pModel.GetMeshCount;

    // are indexes out of bounds?
    if ((index > meshCount) or (nextIndex > meshCount)) then
        Exit;

    pMeshToDraw     := nil;
    pNextMeshToDraw := nil;
    interpolated    := False;

    try
        // get frame to draw, and frame to interpolate with
        pFrame     := GetFrame(index,     pModel, collisions);
        pNextFrame := GetFrame(nextIndex, pModel, collisions);

        // do use shader?
        if (not useShader) then
        begin
            New(pMeshToDraw);

            // interpolate and get next mesh to draw
            TQRModelHelper.Interpolate(interpolationFactor,
                                       pFrame.m_pMesh,
                                       pNextFrame.m_pMesh,
                                       pMeshToDraw^);

            interpolated := True;
        end
        else
        begin
            // get meshes to send to shader
            pMeshToDraw     := @pFrame.m_pMesh;
            pNextMeshToDraw := @pNextFrame.m_pMesh;
        end;

        // get aligned-axis bounding box tree to use to detect collisions
        pAABBTree := pFrame.m_pAABBTree;

        // do use shader?
        if (not useShader) then
            // draw mesh
            TQROpenGLHelper.Draw(pMeshToDraw^, matrix, textures)
        else
        begin
            // prepare shader to draw the model
            PrepareShaderToDrawModel(m_pTextureShader, textures);

            // draw mesh
            TQROpenGLHelper.Draw(pMeshToDraw^,
                                 pNextMeshToDraw^,
                                 matrix,
                                 interpolationFactor,
                                 textures,
                                 m_pTextureShader);
        end;
    finally
        if (interpolated) then
            Dispose(pMeshToDraw);
    end;

    DetectAndDrawCollisions(matrix, pAABBTree, useShader, collisions);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnIdle(pSender: TObject; var done: Boolean);
begin
    done := False;
    RenderGLScene;
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
    Draw(elapsedTime);

    glFlush;

    // finalize scene
    SwapBuffers(m_hDC);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.Draw(const elapsedTime: Double);
begin
    // draw model
    DrawModel(m_pMD3,
              m_Textures,
              m_ModelMatrix,
              0,
              0,
              m_InterpolationFactor,
              m_UseShader,
              m_Collisions);
end;
//--------------------------------------------------------------------------------------------------

end.
