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
 @abstract(@name contains the MD2 demo main form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit Main;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     System.Generics.Collections,
     System.Variants,
     Vcl.Graphics,
     Vcl.ExtCtrls,
     Vcl.ComCtrls,
     Vcl.Controls,
     Vcl.Forms,
     Vcl.Dialogs,
     Vcl.Menus,
     Winapi.Messages,
     Winapi.Windows,
     UTQRFiles,
     UTQR3D,
     UTQRGeometry,
     UTQRCollision,
     UTQRGraphics,
     UTQRModel,
     UTQRModelGroup,
     UTQRMD2,
     UTQRMD2ModelGroup,
     UTQRModelRenderer,
     UTQRThreading,
     UTQROpenGLHelper,
     UTQRShaderOpenGL,
     UTOptions,
     {$IF CompilerVersion <= 25}
         // for compiler until XE4 (not sure until which version), the DelphiGL library is required,
         // because the OpenGL include provided by Embarcadero is incomplete
         XE7.OpenGL,
         XE7.OpenGLext;
     {$ELSE}
         Winapi.OpenGL,
         Winapi.OpenGLext;
     {$ENDIF}

type
    {**
     MD2 demo main form
    }
    TMainForm = class(TForm)
        published
            pbLoadModel: TProgressBar;
            paRendering: TPanel;
            pmOptions: TPopupMenu;
            miPrevAnim: TMenuItem;
            miNextAnim: TMenuItem;

            procedure FormCreate(pSender: TObject);
            procedure FormResize(pSender: TObject);
            procedure FormKeyPress(pSender: TObject; var Key: Char);
            procedure FormPaint(pSender: TObject);
            procedure miPrevAnimClick(pSender: TObject);
            procedure miNextAnimClick(pSender: TObject);

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

        private
            m_pFrames:          IFrames;
            m_pOptions:         TOptions;
            m_hDC:              THandle;
            m_hRC:              THandle;
            m_pMD2:             TQRMD2Group;
            m_pTextureShader:   TQRShaderOpenGL;
            m_pColorShader:     TQRShaderOpenGL;
            m_ProjectionMatrix: TQRMatrix4x4;
            m_ViewMatrix:       TQRMatrix4x4;
            m_PreviousTime:     NativeUInt;
            m_Gesture:          Integer;
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
             Loads MD2 model
             @param(toggleLight If @true, pre-calculated light will be toggled)
             @return(@true on success, otherwise @false)
            }
            function LoadModel(toggleLight: Boolean): Boolean;

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
            function GetFrame(index: NativeUInt; pModel: TQRMD2Model; useCollision: Boolean): IFrame;

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
             @param(textures Textures belonging to model)
            }
            procedure PrepareShaderToDrawModel(pShader: TQRShaderOpenGL; const textures: TQRTextures);

            {**
             Called when mesh texture should be loaded
             @param(pModel Model for which texture should be loaded)
             @param(pBitmap Whenever possible, the bitmap containing the texture, @nil if not available)
             @param(pTexture Texture info)
             @param(loadNext @bold([out]) If @true, event will be called again with a new item to load next texture)
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

    m_pFrames        := IFrames.Create([doOwnsValues]);
    m_pOptions       := nil;
    m_hDC            := 0;
    m_hRC            := 0;
    m_pMD2           := nil;
    m_pTextureShader := nil;
    m_pColorShader   := nil;
    m_PreviousTime   := GetTickCount;
    m_Gesture        := 0;
    m_AnimCached     := False;
    m_Cached         := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.Destroy;
begin
    m_pFrames.Free;

    // delete texture shader
    m_pTextureShader.Free;

    // delete color shader
    m_pColorShader.Free;

    // delete MD2 model
    m_pMD2.Free;

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

    if (m_pOptions.IsAppClosing()) then
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
        MessageDlg('OpenGL could not be initialized.\r\n\r\nApplication will close.',
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

    // load MD2 model
    if (not LoadModel(m_pOptions.ckPreCalculateLight.Checked)) then
    begin
        MessageDlg('Failed to load MD2 model.\r\n\r\nApplication will close.',
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
    m_ProjectionMatrix := TQRModelRenderer.GetPerspective(fov,
                                                          aspect,
                                                          zNear,
                                                          zFar,
                                                          m_pOptions.ckUseOrthoMatrix.Checked);

    position  := Default(TQRVector3D);
    direction := TQRVector3D.Create(0.0, 0.0, 1.0);
    up        := TQRVector3D.Create(0.0, 1.0, 0.0);

    // create view matrix (will not be modified while execution)
    m_ViewMatrix := TQROpenGLHelper.LookAtLH(position, direction, up);

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
procedure TMainForm.miPrevAnimClick(pSender: TObject);
begin
    Dec(m_Gesture);

    if (m_Gesture < 0) then
        m_Gesture := 19;

    m_pMD2.Gesture := m_Gesture;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.miNextAnimClick(pSender: TObject);
begin
    Inc(m_Gesture);

    if (m_Gesture > 19) then
        m_Gesture := 0;

    m_pMD2.Gesture := m_Gesture;
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
function TMainForm.LoadModel(toggleLight: Boolean): Boolean;
var
    hPackageInstance:   THandle;
    pVertexPrg,
    pFragmentPrg,
    pModelStream,
    pNTStream,
    pAnimCfgStream,
    pTextureStream:     TResourceStream;
    pModelColor,
    pAmbient,
    pColor:             TQRColor;
    direction:          TQRVector3D;
    pLight:             TQRMD2Light;
    pTexture:           TQRTexture;
    pMemDir:            TQRMemoryDir;
    modelOptions:       TQRModelOptions;
    framedModelOptions: TQRFramedModelOptions;
begin
    m_pFrames.Clear;

    // get module instance at which this form belongs
    hPackageInstance := FindClassHInstance(TMainForm);

    // found it?
    if (hPackageInstance = 0) then
    begin
        Result := False;
        Exit;
    end;

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
                begin
                    Result := False;
                    Exit;
                end;
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
                begin
                    Result := False;
                    Exit;
                end;
            finally
                // delete resource streams, if needed
                pVertexPrg.Free;
                pFragmentPrg.Free;
            end;
    end;

    pAmbient       := nil;
    pColor         := nil;
    pModelColor    := nil;
    pLight         := nil;
    pModelStream   := nil;
    pNTStream      := nil;
    pAnimCfgStream := nil;
    pTextureStream := nil;
    pMemDir        := nil;

    try
        // create MD2 model, if needed
        if (not Assigned(m_pMD2)) then
        begin
            m_pMD2                   := TQRMD2Group.Create;
            m_pMD2.OnLoadMeshTexture := OnLoadMeshTexture;
            m_pMD2.OnDrawItem        := OnDrawModelItem;
            m_pMD2.OnCustomDrawItem  := OnDrawCustomModelItem;
        end;

        // set basic configuration (normals are not required here as the pre-calculated lights are
        // calculated and embedded inside the colors buffer directly)
        modelOptions := [EQR_MO_Without_Normals];

        // dispatch caching type
        case (m_pOptions.rgCacheOptions.ItemIndex) of
            0:
            begin
                Include(modelOptions, EQR_MO_Create_Cache);

                // do show default frame while cache is created?
                if (m_pOptions.ckShowDefaultFrame.Checked) then
                    Include(framedModelOptions, EQR_FO_Show_Default_Frame);

                // do run currently selected gesture when available?
                if (m_pOptions.ckRunGestureWhenReady.Checked) then
                    Include(framedModelOptions, EQR_FO_Start_Anim_When_Gesture_Is_Ready);
            end;

            1: Include(modelOptions, EQR_MO_Dynamic_Frames_No_Cache);
            2: Include(modelOptions, EQR_MO_Dynamic_Frames);
            3: begin end;
        else
            Result := False;
            Exit;
        end;

        // if shader is used, interpolation will be done on the shader side
        if (not m_pOptions.ckUseShader.Checked) then
            Include(framedModelOptions, EQR_FO_Interpolate);

         // do toggle light?
        if (toggleLight) then
        begin
            pAmbient := TQRColor.Create(32, 32, 32, 255);
            pColor   := TQRColor.Create(255, 255, 255, 255);

            // configure precalculated light
            pLight            := TQRMD2Light.Create;
            pLight.Ambient    := pAmbient;
            pLight.Color      := pColor;
            pLight.Direction^ := TQRVector3D.Create(1.0, 0.0, 0.0);
            pLight.Enabled    := True;
        end;

        // load model from resources
        if (FindResource(hPackageInstance, PChar('ID_MD2_MODEL'), RT_RCDATA) <> 0)
        then
            pModelStream := TResourceStream.Create(hPackageInstance,
                                                   PChar('ID_MD2_MODEL'),
                                                   RT_RCDATA);

        // load normals from resources
        if (FindResource(hPackageInstance, PChar('ID_MD2_NORMALS_TABLE'), RT_RCDATA) <> 0)
        then
            pNTStream := TResourceStream.Create(hPackageInstance,
                                                PChar('ID_MD2_NORMALS_TABLE'),
                                                RT_RCDATA);

        // load animation configuration from resources
        if (FindResource(hPackageInstance, PChar('ID_MD2_ANIM_CFG'), RT_RCDATA) <> 0)
        then
            pAnimCfgStream := TResourceStream.Create(hPackageInstance,
                                                     PChar('ID_MD2_ANIM_CFG'),
                                                     RT_RCDATA);

        // load texture from resources
        if (FindResource(hPackageInstance, PChar('ID_MD2_TEXTURE'), RT_RCDATA) <> 0)
        then
            pTextureStream := TResourceStream.Create(hPackageInstance,
                                                     PChar('ID_MD2_TEXTURE'),
                                                     RT_RCDATA);

        pMemDir := TQRMemoryDir.Create(True);

        if (not pMemDir.AddFile('marvin.md2', pModelStream, False)) then
        begin
            Result := False;
            Exit;
        end;

        pModelStream := nil;

        if (not pMemDir.AddFile('marvin.bin', pNTStream, False)) then
        begin
            Result := False;
            Exit;
        end;

        pNTStream := nil;

        if (not pMemDir.AddFile('marvin.cfg', pAnimCfgStream, False)) then
        begin
            Result := False;
            Exit;
        end;

        pAnimCfgStream := nil;

        if (not pMemDir.AddFile('marvin.bmp', pTextureStream, False)) then
        begin
            Result := False;
            Exit;
        end;

        pTextureStream := nil;

        pModelColor := TQRColor.Create(255, 255, 255, 255);

        // load model
        if (not m_pMD2.Load(pMemDir,
                            'marvin',
                            pModelColor,
                            pLight,
                            False,
                            modelOptions,
                            framedModelOptions))
        then
        begin
            Result := False;
            Exit;
        end;

        pMemDir := nil;

        if (toggleLight) then
            pLight := nil;

        // orthogonal matrix is used?
        if (m_pOptions.ckUseOrthoMatrix.Checked) then
        begin
            // translate and scale model
            m_pMD2.Translation^ := TQRVector3D.Create(0.0,   0.0,   -1.5);
            m_pMD2.Scaling^     := TQRVector3D.Create(0.015, 0.015,  0.015);
        end
        else
        begin
            // translate and scale model
            m_pMD2.Translation^ := TQRVector3D.Create(0.0,  0.05, -1.5);
            m_pMD2.Scaling^     := TQRVector3D.Create(0.02, 0.02,  0.02);
        end;

        // rotate model
        m_pMD2.RotationX := -(PI / 2.0); // -90°
        m_pMD2.RotationZ := -(PI / 4.0); // -45°

        // set gesture to run
        m_pMD2.Gesture := 0;
    finally
        pAmbient.Free;
        pColor.Free;
        pModelColor.Free;
        pLight.Free;
        pModelStream.Free;
        pNTStream.Free;
        pAnimCfgStream.Free;
        pTextureStream.Free;
        pMemDir.Free;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.UpdateCacheProgress;
var
    pJobStatus: TQRModelJobStatus;
begin
    // get job status
    pJobStatus := m_pMD2.QueryJobStatus;

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
function TMainForm.GetFrame(index: NativeUInt; pModel: TQRMD2Model; useCollision: Boolean): IFrame;
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
                                              const pAABBTree: TQRAABBTree);
var
    rect:                                        TQRRect;
    rayPos, rayDir:                              TQRVector3D;
    invertMatrix:                                TQRMatrix4x4;
    determinant:                                 Single;
    pRay:                                        TQRRay;
    mesh:                                        TQRMesh;
    polygons, polygonToDraw:                     TQRPolygons;
    textures:                                    TQRTextures;
    polygonCount, polygonToDrawCount, i, offset: NativeUInt;
    uniform:                                     GLint;
begin
    if ((not m_pOptions.ckShowCollisions.Checked) or (not Assigned(pAABBTree))) then
        Exit;

    // calculate client rect in OpenGL coordinates
    rect := TQRRect.Create(-1.0, 1.0, 2.0, 2.0);

    // convert mouse position to OpenGL point, that will be used as ray start pos, and create ray dir
    rayPos := TQROpenGLHelper.MousePosToGLPoint(Handle, rect);
    rayDir := TQRVector3D.Create(0.0, 0.0, 1.0);

    // orthogonal matrix is used?
    if (not m_pOptions.ckUseOrthoMatrix.Checked) then
    begin
        // update ray position to match with the model
        rayPos.Y := rayPos.Y + 0.025;

        // this is a lazy way to correct a perspective issue. In fact, the model is much larger than
        // its image on the screen, but it is placed very far in relation to the screen. In the model
        // coordinates, the ray location is beyond the mouse coordinate. For that, a ratio is needed
        // to keep the ray coordinates coherent with the mouse position. Not ideal (e.g. the model
        // feet are not always well detected), but this is efficient for the majority of cases
        rayPos.MulAndAssign(1.35);
    end;

    // transform the ray to be on the same coordinates system as the model
    invertMatrix := modelMatrix.Multiply(m_ViewMatrix).Multiply(m_ProjectionMatrix).Inverse(determinant);
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
                // add polygon in collision to resulting list
                SetLength(polygonToDraw, Length(polygonToDraw) + 1);
                polygonToDraw[Length(polygonToDraw) - 1] := polygons[i];
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
    begin
        Result := False;
        Exit;
    end;

    if (not Assigned(pTexture)) then
    begin
        Result := False;
        Exit;
    end;

    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

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
        PrepareShaderToDrawModel(m_pTextureShader, textures);

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
    pMD2Model:                    TQRMD2Model;
    meshCount:                    NativeInt;
    pMeshToDraw, pNextMeshToDraw: PQRMesh;
    pAABBTree:                    TQRAABBTree;
    interpolated:                 Boolean;
    pFrame, pNextFrame:           IFrame;
begin
    // no model to draw?
    if (not Assigned(pModel)) then
        Exit;

    // get MD2 model
    pMD2Model := TQRMD2Model(pModel);

    // found it?
    if (not Assigned(pMD2Model)) then
        Exit;

    // get mesh count
    meshCount := pMD2Model.GetMeshCount();

    // are indexes out of bounds?
    if ((index > NativeInt(meshCount)) or (nextIndex > NativeInt(meshCount))) then
        Exit;

    pMeshToDraw     := nil;
    pNextMeshToDraw := nil;
    interpolated    := False;

    try
        // get frame to draw, and frame to interpolate with
        pFrame     := GetFrame(index,     pMD2Model, m_pOptions.ckShowCollisions.Checked);
        pNextFrame := GetFrame(nextIndex, pMD2Model, m_pOptions.ckShowCollisions.Checked);

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
        if (not m_pOptions.ckUseShader.Checked) then
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
    m_pMD2.Draw(elapsedTime);
end;
//--------------------------------------------------------------------------------------------------

end.
