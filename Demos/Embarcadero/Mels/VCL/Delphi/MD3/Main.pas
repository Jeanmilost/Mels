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
     System.SysUtils,
     System.Math,
     System.Generics.Collections,
     System.Variants,
     System.UITypes,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.ExtCtrls,
     Vcl.ComCtrls,
     Vcl.Forms,
     Vcl.Menus,
     Vcl.Dialogs,
     Winapi.Windows,
     Winapi.Messages,
     Winapi.OpenGL,
     {$IF CompilerVersion <= 25}
         // for compiler until XE4 (not sure until which version), the DelphiGL library is required,
         // because the OpenGL include provided by Embarcadero is incomplete
         DelphiGL.OpenGL,
         DelphiGL.OpenGLext,
     {$ELSE}
         Winapi.OpenGLext,
     {$ENDIF}
     UTQRGraphics,
     UTQR3D,
     UTQRGeometry,
     UTQRRenderer,
     UTQRCollision,
     UTQRThreading,
     UTQRModel,
     UTQRMD3,
     UTQRModelGroup,
     UTQRMD3ModelGroup,
     UTQROpenGLHelper,
     UTQRShaderOpenGL,
     UTOptions;

type
    {**
     MD3 demo main form
    }
    TMainForm = class(TForm)
        published
            pbLoadModel: TProgressBar;
            paRendering: TPanel;
            pmOptions: TPopupMenu;
            miPrevTorsoAnim: TMenuItem;
            miNextTorsoAnim: TMenuItem;
            miSeparator: TMenuItem;
            miPrevLegsAnim: TMenuItem;
            miNextLegsAnim: TMenuItem;

            procedure FormCreate(pSender: TObject);
            procedure FormResize(pSender: TObject);
            procedure FormKeyPress(pSender: TObject; var Key: Char);
            procedure FormPaint(pSender: TObject);
            procedure miPrevTorsoAnimClick(pSender: TObject);
            procedure miNextTorsoAnimClick(pSender: TObject);
            procedure miPrevLegsAnimClick(pSender: TObject);
            procedure miNextLegsAnimClick(pSender: TObject);

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
            ICache  = TObjectDictionary<TQRModel,   IFrames>;

        private
            m_pCache:           ICache;
            m_pOptions:         TOptions;
            m_hDC:              THandle;
            m_hRC:              THandle;
            m_pMD3:             TQRMD3Group;
            m_pTextureShader:   TQRShaderOpenGL;
            m_pColorShader:     TQRShaderOpenGL;
            m_ProjectionMatrix: TQRMatrix4x4;
            m_ViewMatrix:       TQRMatrix4x4;
            m_PreviousTime:     NativeUInt;
            m_CurTorsoGesture:  Integer;
            m_CurLegsGesture:   Integer;
            m_AnimCached:       Boolean;
            m_Cached:           Boolean;

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
            function BuildShader(pVertexPrg, pFragmentPrg: TStream; pShader: TQRShaderOpenGL): Boolean;

            {**
             Loads MD3 model
             @return(@true on success, otherwise @false)
            }
            function LoadModel: Boolean;

            {**
             Updates cache progress bar
            }
            procedure UpdateCacheProgress;

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
            }
            procedure DetectAndDrawCollisions(const modelMatrix: TQRMatrix4x4;
                                                const pAABBTree: TQRAABBTree);

            {**
             Prepres the shader to draw the model
             @param(pShader Shader to prepare)
            *@param(modelName Model name)
             @param(textures Textures belonging to model)
            }
            procedure PrepareShaderToDrawModel(pShader: TQRShaderOpenGL;
                                       const modelName: UnicodeString;
                                        const textures: TQRTextures);

            {**
             Called when mesh texture should be loaded
             @param(pModel Model for which texture should be loaded)
             @param(pBitmap Whenever possible, the bitmap containing the texture, @nil if not available)
             @param(pTexture Texture info)
             @param(loadNext @bold([out]) If @true, event will be called again with a new item to
                                          load next texture)
             @return(@true on success, otherwise @false)
            }
            function OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                       const pModel: TQRModel;
                                            pBitmap: Vcl.Graphics.TBitmap;
                                           pTexture: TQRTexture;
                                       out loadNext: Boolean): Boolean;

            {**
             Called when framed model item should be drawn
             @param(pGroup Group at which model belongs)
             @param(pModel Model to draw)
             @param(textures Textures belonging to model, in the order where they should be combined)
             @param(matrix Model matrix)
             @param(index Model mesh index)
             @param(nextIndex Model mesh index to interpolate with)
             @param(interpolationFactor Interpolation factor)
             @param(pMesh Mesh to draw, can be @nil)
             @param(pNextMesh Next mesh to interpolate with, can be @nil)
             @param(pAABBTree Aligned-axis bounding box tree matching with mesh, can be @nil)
             @param(pNextAABBTree Aligned-axis bounding box tree matching with next mesh, can be @nil)
            }
            procedure OnDrawModelItem(const pGroup: TQRModelGroup;
                                             const pModel: TQRModel;
                                           const textures: TQRTextures;
                                             const matrix: TQRMatrix4x4;
                                         index, nextIndex: NativeInt;
                                const interpolationFactor: Double;
                                   const pMesh, pNextMesh: PQRMesh;
                           const pAABBTree, pNextAABBTree: TQRAABBTree);

            {**
             Called when framed model item should be drawn
             @param(pGroup Group at which model belongs)
             @param(pModel Model to draw)
             @param(textures Textures belonging to model, in the order where they should be combined)
             @param(matrix Model matrix)
             @param(index Model mesh index)
             @param(nextIndex Model mesh index to interpolate with)
             @param(interpolationFactor Interpolation factor)
            }
            procedure OnDrawCustomModelItem(const pGroup: TQRModelGroup;
                                                  pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4;
                                        index, nextIndex: NativeInt;
                               const interpolationFactor: Double);

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
// TMainForm
//--------------------------------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_pCache           := ICache.Create([doOwnsValues]);
    m_pOptions         := nil;
    m_hDC              := 0;
    m_hRC              := 0;
    m_pMD3             := nil;
    m_pTextureShader   := nil;
    m_pColorShader     := nil;
    m_PreviousTime     := GetTickCount;
    m_CurTorsoGesture  := 0;
    m_CurLegsGesture   := 0;
    m_AnimCached       := False;
    m_Cached           := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.Destroy;
begin
    m_pCache.Free;

    // delete texture shader
    m_pTextureShader.Free;

    // delete color shader
    m_pColorShader.Free;

    // delete MD3 model
    m_pMD3.Free;

    // shutdown OpenGL
    TQROpenGLHelper.DisableOpenGL(Handle, m_hDC, m_hRC);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormCreate(pSender: TObject);
begin
    // create and show options to user
    m_pOptions := TOptions.Create(Self);
    m_pOptions.ShowModal;

    if (m_pOptions.IsAppClosing) then
        Exit;

    // do show model in full screen?
    if (m_pOptions.ckFullScreen.Checked) then
    begin
        BorderStyle := bsNone;
        WindowState := wsMaximized;
        ShowCursor(m_pOptions.ckShowCollisions.Checked and (m_pOptions.rgCacheOptions.ItemIndex <> 1));
    end
    else
    begin
        BorderStyle := bsSizeable;
        WindowState := wsNormal;
        ShowCursor(True);
    end;

    BringToFront;

    // select correct cursor to use
    if (m_pOptions.ckShowCollisions.Checked) then
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
    if (m_pOptions.ckUseShader.Checked) then
        InitOpenGLext;

    // configure OpenGL
    ConfigOpenGL;

    // load MD3 model
    if (not LoadModel) then
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
    position, direction, up:      TQRVector3D;
    fov, zNear, zFar, maxX, maxY: Single;
    widthF, heightF, aspect:      GLfloat;
begin
    fov   := 45.0;
    zNear := 1.0;
    zFar  := 200.0;

    widthF  := ClientWidth;
    heightF := ClientHeight;

    // is width out of bounds?
    if (widthF = 0.0) then
        widthF := 1.0;

    // is height out of bounds?
    if (heightF = 0.0) then
        heightF := 1.0;

    aspect  := widthF / heightF;

    // create projection matrix (will not be modified while execution)
    m_ProjectionMatrix := TQRRenderer.GetPerspective(fov,
                                                     aspect,
                                                     zNear,
                                                     zFar,
                                                     m_pOptions.ckUseOrthoMatrix.Checked);

    position  := Default(TQRVector3D);
    direction := TQRVector3D.Create(0.0, 0.0, 1.0);
    up        := TQRVector3D.Create(0.0, 1.0, 0.0);

    // create view matrix (will not be modified while execution)
    m_ViewMatrix := TQRRenderer.LookAtLH(position, direction, up);

    TQROpenGLHelper.CreateViewport(ClientWidth, ClientHeight, not m_pOptions.ckUseShader.Checked);

    // configure matrices for OpenGL direct mode
    if (m_pOptions.ckUseOrthoMatrix.Checked) then
    begin
        maxY := zNear * Tan(fov * PI / 360.0);
        maxX := maxY  * aspect;

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity;
        glOrtho(-maxX, maxX, -maxY, maxY, zNear, zFar);
        glMatrixMode(GL_MODELVIEW);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormKeyPress(pSender: TObject; var Key: Char);
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
procedure TMainForm.miPrevTorsoAnimClick(pSender: TObject);
begin
    Dec(m_CurTorsoGesture);

    if (m_CurTorsoGesture < Integer(EQR_AG_MD3_Both_Death1)) then
        m_CurTorsoGesture := Integer(EQR_AG_MD3_Torso_Stand2);

    // set new torso animation gesture
    m_pMD3.SetAnimation('upper', EQRMD3AnimationGesture(m_CurTorsoGesture));
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.miNextTorsoAnimClick(pSender: TObject);
begin
    Inc(m_CurTorsoGesture);

    if (m_CurTorsoGesture > Integer(EQR_AG_MD3_Torso_Stand2)) then
        m_CurTorsoGesture := Integer(EQR_AG_MD3_Both_Death1);

    // set new torso animation gesture
    m_pMD3.SetAnimation('upper', EQRMD3AnimationGesture(m_CurTorsoGesture));
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.miPrevLegsAnimClick(pSender: TObject);
begin
    Dec(m_CurLegsGesture);

    if (m_CurLegsGesture < Integer(EQR_AG_MD3_Both_Death1)) then
        m_CurLegsGesture := Integer(EQR_AG_MD3_Legs_Turn);

    if ((m_CurLegsGesture > Integer(EQR_AG_MD3_Both_Dead3)) and
        (m_CurLegsGesture < Integer(EQR_AG_MD3_Legs_Walk_Crouching)))
    then
        m_CurLegsGesture := Integer(EQR_AG_MD3_Both_Dead3);

    // set new legs animation gesture
    m_pMD3.SetAnimation('lower', EQRMD3AnimationGesture(m_CurLegsGesture));
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.miNextLegsAnimClick(pSender: TObject);
begin
    Inc(m_CurLegsGesture);

    if (m_CurLegsGesture > Integer(EQR_AG_MD3_Legs_Turn)) then
        m_CurLegsGesture := Integer(EQR_AG_MD3_Both_Death1);

    if ((m_CurLegsGesture > Integer(EQR_AG_MD3_Both_Dead3)) and
        (m_CurLegsGesture < Integer(EQR_AG_MD3_Legs_Walk_Crouching)))
    then
        m_CurLegsGesture := Integer(EQR_AG_MD3_Legs_Walk_Crouching);

    // set new legs animation gesture
    m_pMD3.SetAnimation('lower', EQRMD3AnimationGesture(m_CurLegsGesture));
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
function TMainForm.BuildShader(pVertexPrg, pFragmentPrg: TStream; pShader: TQRShaderOpenGL): Boolean;
begin
    // load and compile shader
    pShader.CreateProgram;
    pShader.AttachFile(pVertexPrg,   EQR_ST_Vertex);
    pShader.AttachFile(pFragmentPrg, EQR_ST_Fragment);

    // try to link shader
    Result := pShader.Link(False);
end;
//--------------------------------------------------------------------------------------------------
function TMainForm.LoadModel: Boolean;
var
    hPackageInstance:   THandle;
    pVertexPrg,
    pFragmentPrg,
    pModelStream:       TResourceStream;
    pFileStream:        TFileStream;
    pColor:             TQRColor;
    modelOptions:       TQRModelOptions;
    framedModelOptions: TQRFramedModelOptions;
begin
    m_pCache.Clear;

    // get module instance at which this form belongs
    hPackageInstance := FindClassHInstance(TMainForm);

    // found it?
    if (hPackageInstance = 0) then
        Exit(False);

    // do use shader?
    if (m_pOptions.ckUseShader.Checked) then
    begin
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

                // found resource containing the fragment shader program to load?
                if (FindResource(hPackageInstance, PChar('ID_TEXTURE_FRAGMENT_SHADER'), RT_RCDATA) <> 0)
                then
                    pFragmentPrg := TResourceStream.Create(hPackageInstance,
                                                         PChar('ID_TEXTURE_FRAGMENT_SHADER'),
                                                         RT_RCDATA);

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

                // found resource containing the fragment shader program to load?
                if (FindResource(hPackageInstance, PChar('ID_COLOR_FRAGMENT_SHADER'), RT_RCDATA) <> 0)
                then
                    pFragmentPrg := TResourceStream.Create(hPackageInstance,
                                                           PChar('ID_COLOR_FRAGMENT_SHADER'),
                                                           RT_RCDATA);

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
    end;

    // create MD3 model, if needed
    if (not Assigned(m_pMD3)) then
    begin
        m_pMD3                   := TQRMD3Group.Create;
        m_pMD3.OnLoadMeshTexture := OnLoadMeshTexture;
        m_pMD3.OnDrawItem        := OnDrawModelItem;
        m_pMD3.OnCustomDrawItem  := OnDrawCustomModelItem;
    end;

    // set basic configuration (normals are not required here as the lighting is not used for now)
    modelOptions := [EQR_MO_Without_Normals];

    // dispatch caching type
    case (m_pOptions.rgCacheOptions.ItemIndex) of
        0: Include(modelOptions, EQR_MO_Create_Cache);
        1: Include(modelOptions, EQR_MO_Dynamic_Frames_No_Cache);
        2: Include(modelOptions, EQR_MO_Dynamic_Frames);
        3: begin end;
    else
        Exit(False);
    end;

    // if shader is used, interpolation will be done on the shader side
    if (not m_pOptions.ckUseShader.Checked) then
        Include(framedModelOptions, EQR_FO_Interpolate);

    // do load custom MD3 model?
    if (Length(m_pOptions.edModelFileName.Text) > 0) then
    begin
        pFileStream := nil;
        pColor      := nil;

        try
            pColor := TQRColor.Create(255, 255, 255, 255);

            // open package stream
            pFileStream := TFileStream.Create(m_pOptions.edModelFileName.Text, fmOpenRead);

            if (not m_pMD3.Load(pFileStream,
                                pColor,
                                False,
                                modelOptions,
                                framedModelOptions))
            then
            begin
                pFileStream := nil;
                Exit(False);
            end;

            pFileStream := nil;
        finally
            pFileStream.Free;
            pColor.Free;
        end;
    end
    else
    begin
        pModelStream := nil;
        pColor       := nil;

        try
            pColor := TQRColor.Create(255, 255, 255, 255);

            // load model from resources
            if (FindResource(hPackageInstance, PChar('ID_MD3_MODEL'), RT_RCDATA) <> 0)
            then
                pModelStream := TResourceStream.Create(hPackageInstance,
                                                       PChar('ID_MD3_MODEL'),
                                                       RT_RCDATA);

            // found it?
            if (not Assigned(pModelStream)) then
                Exit(False);

            // load model from resources
            if (not m_pMD3.Load(pModelStream,
                                pColor,
                                False,
                                modelOptions,
                                framedModelOptions,
                                m_pOptions.GetSelectedTeam))
            then
            begin
                pModelStream := nil;
                Exit(False);
            end;

            pModelStream := nil;
        finally
            pModelStream.Free;
            pColor.Free;
        end;
    end;

    // orthogonal matrix is used?
    if (m_pOptions.ckUseOrthoMatrix.Checked) then
    begin
        // translate and scale model
        m_pMD3.Translation^ := TQRVector3D.Create(0.0,   -0.06,  -1.5);
        m_pMD3.Scaling^     := TQRVector3D.Create(0.011,  0.011,  0.011);
    end
    else
    begin
        // translate and scale model
        m_pMD3.Translation^ := TQRVector3D.Create(0.0,   -0.1,   -1.5);
        m_pMD3.Scaling^     := TQRVector3D.Create(0.015,  0.015,  0.015);
    end;

    // rotate model
     m_pMD3.RotationX := -PI / 2.0; // -90°
     m_pMD3.RotationY := -PI / 4.0; // -45°

    m_CurTorsoGesture := Integer(EQR_AG_MD3_Torso_Stand);
    m_CurLegsGesture  := Integer(EQR_AG_MD3_Legs_Walk);

    // set default animation gesture
    m_pMD3.SetAnimation('upper', EQRMD3AnimationGesture(m_CurTorsoGesture));
    m_pMD3.SetAnimation('lower', EQRMD3AnimationGesture(m_CurLegsGesture));

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.UpdateCacheProgress;
var
    pJobStatus: TQRModelJobStatus;
begin
    // get job status
    pJobStatus := m_pMD3.QueryJobStatus;

    // found it?
    if (not Assigned(pJobStatus)) then
    begin
        pbLoadModel.Visible := False;
        Exit;
    end;

    // job was termined?
    if ((pJobStatus.Status = EQR_JS_Done) or (pJobStatus.Status = EQR_JS_Error)) then
    begin
        pbLoadModel.Visible := False;
        Exit;
    end;

    // show job progress
    pbLoadModel.Visible  := True;
    pbLoadModel.Max      := 100;
    pbLoadModel.Position := pJobStatus.Progress;
end;
//--------------------------------------------------------------------------------------------------
function TMainForm.GetFrame(index: NativeUInt; pModel: TQRMD3Model; useCollision: Boolean): IFrame;
var
    pFrames:    IFrames;
    frameAdded: Boolean;
begin
    frameAdded := False;
    Result     := nil;

    // search for model in cache (a md3 contains many sub-models)
    if (not m_pCache.TryGetValue(pModel, pFrames)) then
    begin
        pFrames := nil;

        try
            // create new frame item, and populate it
            Result := IFrame.Create(useCollision);
            pModel.GetMesh(index, Result.m_pMesh, Result.m_pAABBTree);

            // create new model item, and populate it
            pFrames := IFrames.Create([doOwnsValues]);
            pFrames.Add(index, Result);

            // cache current model frame
            m_pCache.Add(pModel, pFrames);
            frameAdded := True;
        finally
            if (not frameAdded) then
            begin
                pFrames.Free;
                Result.Free;
                Result := nil;
            end;
        end;

        Exit;
    end;

    // search for cached frame in model cache
    if (pFrames.TryGetValue(index, Result)) then
        Exit;

    try
        Result := IFrame.Create(useCollision);

        // create new frame item, populate it, and add it to cache
        pModel.GetMesh(index, Result.m_pMesh, Result.m_pAABBTree);
        pFrames.Add(index, Result);
        frameAdded := True;
    finally
        if (not frameAdded) then
        begin
            Result.Free;
            Result := nil;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.DetectAndDrawCollisions(const modelMatrix: TQRMatrix4x4;
                                              const pAABBTree: TQRAABBTree);
var
    rect:                    TQRRect;
    rayPos, rayDir:          TQRVector3D;
    invertModel:             TQRMatrix4x4;
    determinant:             Single;
    pRay:                    TQRRay;
    mesh:                    TQRMesh;
    polygons, polygonToDraw: TQRPolygons;
    polygon:                 TQRPolygon;
    textures:                TQRTextures;
    polygonCount, offset:    NativeUInt;
    uniform:                 GLint;
begin
    if ((not m_pOptions.ckShowCollisions.Checked) or (not Assigned(pAABBTree))) then
        Exit;

    // calculate client rect in OpenGL coordinates
    rect := TQRRect.Create(-1.0, 1.0, 2.0, 2.0);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    rayPos := TQROpenGLHelper.MousePosToGLPoint(Handle, rect);

    // orthogonal matrix is used?
    if (not m_pOptions.ckUseOrthoMatrix.Checked) then
        rayDir := TQRVector3D.Create(rayPos.X, rayPos.Y, 1.0)
    else
        rayDir := TQRVector3D.Create(0.0, 0.0, 1.0);

    // unproject the ray to make it inside the 3d world coordinates
    TQRRenderer.Unproject(m_ProjectionMatrix, m_ViewMatrix, rayPos, rayDir);

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

    polygonCount := Length(polygonToDraw);

    // found polgons to draw?
    if (polygonCount = 0) then
        Exit;

    SetLength(mesh, 1);

    mesh[0].m_Type      := EQR_VT_Triangles;
    mesh[0].m_CoordType := EQR_VC_XYZ;
    mesh[0].m_Stride    := 7;
    mesh[0].m_Format    := [EQR_VF_Colors];
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
        Inc(offset, (mesh[0].m_Stride * 3));
    end;

    // do use shader?
    if (m_pOptions.ckUseShader.Checked) then
    begin
        // bind shader program
        m_pColorShader.Use(True);

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
        m_pColorShader.Use(False);

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
procedure TMainForm.PrepareShaderToDrawModel(pShader: TQRShaderOpenGL;
                                     const modelName: UnicodeString;
                                      const textures: TQRTextures);
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
function TMainForm.OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                     const pModel: TQRModel;
                                          pBitmap: Vcl.Graphics.TBitmap;
                                         pTexture: TQRTexture;
                                     out loadNext: Boolean): Boolean;
var
    pixelFormat: Integer;
    pPixels:     PByte;
begin
    if (not Assigned(pModel)) then
        Exit(False);

    if (not Assigned(pTexture)) then
        Exit(False);

    if (not Assigned(pBitmap)) then
    begin
        pTexture.Enabled := False;
        Exit(True);
    end;

    // make sure texture is a power of 2 texture (OpenGL may not support non POT textures)
    TQRModelGroupHelper.MakeTexturePowerOf2(pBitmap, pBitmap);

    if (pBitmap.PixelFormat = pf32bit) then
        pixelFormat := GL_RGBA
    else
        pixelFormat := GL_RGB;

    pPixels := nil;

    try
        // convert bitmap to pixel array, and create OpenGL texture from array
        TQROpenGLHelper.BytesFromBitmap(pBitmap, pPixels, False, False);
        pTexture.Index := TQROpenGLHelper.CreateTexture(pBitmap.Width,
                                                        pBitmap.Height,
                                                        pixelFormat,
                                                        pPixels,
                                                        GL_NEAREST,
                                                        GL_NEAREST,
                                                        GL_TEXTURE_2D);
    finally
        if (Assigned(pPixels)) then
            FreeMem(pPixels);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnDrawModelItem(const pGroup: TQRModelGroup;
                                    const pModel: TQRModel;
                                  const textures: TQRTextures;
                                    const matrix: TQRMatrix4x4;
                                index, nextIndex: NativeInt;
                       const interpolationFactor: Double;
                          const pMesh, pNextMesh: PQRMesh;
                  const pAABBTree, pNextAABBTree: TQRAABBTree);
var
    mesh:            TQRMesh;
    pNextMeshToDraw: PQRMesh;
begin
    if (not Assigned(pModel)) then
        Exit;

    if (not Assigned(pMesh)) then
        Exit;

    if (not Assigned(pNextMesh)) then
        pNextMeshToDraw := pMesh
    else
        pNextMeshToDraw := pNextMesh;

    // do use shader?
    if (m_pOptions.ckUseShader.Checked) then
    begin
        // prepare shader to draw the model
        PrepareShaderToDrawModel(m_pTextureShader, '', textures);

        // draw mesh
        TQROpenGLHelper.Draw(pMesh^,
                             pNextMeshToDraw^,
                             matrix,
                             interpolationFactor,
                             textures,
                             m_pTextureShader);
    end
    else
    begin
        // get next frame to draw
        TQRModelHelper.Interpolate(interpolationFactor, pMesh^, pNextMeshToDraw^, mesh);

        // draw mesh
        TQROpenGLHelper.Draw(mesh, matrix, textures);
    end;

    DetectAndDrawCollisions(matrix, pAABBTree);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnDrawCustomModelItem(const pGroup: TQRModelGroup;
                                                pModel: TQRModel;
                                        const textures: TQRTextures;
                                          const matrix: TQRMatrix4x4;
                                      index, nextIndex: NativeInt;
                             const interpolationFactor: Double);
var
    pMD3Model:                    TQRMD3Model;
    meshCount:                    NativeInt;
    pMeshToDraw, pNextMeshToDraw: PQRMesh;
    pAABBTree:                    TQRAABBTree;
    interpolated:                 Boolean;
    pFrame, pNextFrame:           IFrame;
begin
    // no model to draw?
    if (not Assigned(pModel)) then
        Exit;

    // get MD3 model
    pMD3Model := TQRMD3Model(pModel);

    // found it?
    if (not Assigned(pMD3Model)) then
        Exit;

    // get mesh count
    meshCount := pMD3Model.GetMeshCount;

    // are indexes out of bounds?
    if ((index > NativeInt(meshCount)) or (nextIndex > NativeInt(meshCount))) then
        Exit;

    pMeshToDraw     := nil;
    pNextMeshToDraw := nil;
    interpolated    := False;

    try
        // get frame to draw, and frame to interpolate with
        pFrame     := GetFrame(index,     pMD3Model, m_pOptions.ckShowCollisions.Checked);
        pNextFrame := GetFrame(nextIndex, pMD3Model, m_pOptions.ckShowCollisions.Checked);

        // do use shader?
        if (not m_pOptions.ckUseShader.Checked) then
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
        if (m_pOptions.ckUseShader.Checked) then
        begin
            // do interpolate meshes on the shader side?
            if ((not interpolated) and Assigned(pNextMeshToDraw)) then
            begin
                // prepare shader to draw the model
                PrepareShaderToDrawModel(m_pTextureShader, '', textures);

                // draw mesh
                TQROpenGLHelper.Draw(pMeshToDraw^,
                                     pNextMeshToDraw^,
                                     matrix,
                                     interpolationFactor,
                                     textures,
                                     m_pTextureShader);
            end
            else
            begin
                // prepare shader to draw the model
                PrepareShaderToDrawModel(m_pTextureShader, '', textures);

                // draw mesh
                TQROpenGLHelper.Draw(pMeshToDraw^, matrix, textures, m_pTextureShader);
            end;
        end
        else
            // draw mesh
            TQROpenGLHelper.Draw(pMeshToDraw^, matrix, textures);
    finally
        if (interpolated) then
            Dispose(pMeshToDraw);
    end;

    DetectAndDrawCollisions(matrix, pAABBTree);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnIdle(pSender: TObject; var done: Boolean);
begin
    done := False;
    RenderGLScene;
    UpdateCacheProgress;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.RenderGLScene;
var
    now:         NativeUInt;
    elapsedTime: Double;
begin
    if ((m_hDC = 0) or (m_hRC = 0)) then
        Exit;

    wglMakeCurrent(m_hDC, m_hRC);

    // calculate time interval
    now            :=  GetTickCount;
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
    m_pMD3.Draw(elapsedTime);
end;
//--------------------------------------------------------------------------------------------------

end.
