// *************************************************************************************************
// * ==> UTOptions --------------------------------------------------------------------------------*
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
 @abstract(@name contains the MD2 demo options form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTOptions;

interface

uses System.Classes,
     System.SysUtils,
     System.Generics.Collections,
     System.Variants,
     System.UITypes,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.ExtCtrls,
     Vcl.StdCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Winapi.Messages,
     Winapi.Windows,
     UTQRSmartPointer,
     UTQR3D,
     UTQRGeometry,
     UTQRGraphics,
     UTQRThreading,
     UTQRModel,
     UTQRModelGroup,
     UTQRMD3,
     UTQRMD3ModelGroup,
     UTQRVCLHelpers,
     UTQROpenGLHelper,
     // for compiler until XE4 (not sure until which version), the DelphiGL library is required,
     // because the OpenGL include provided by Embarcadero is incomplete
     XE7.OpenGL,
     XE7.OpenGLext;

type
    {**
     User options form
    }
    TOptions = class(TForm)
        published
            paPreview: TPanel;
            imPreview: TImage;
            paMain: TPanel;
            gbRenderOptions: TGroupBox;
            ckShowCollisions: TCheckBox;
            ckUseShader: TCheckBox;
            ckFullScreen: TCheckBox;
            ckUseOrthoMatrix: TCheckBox;
            rgCacheOptions: TRadioGroup;
            gbLoadModel: TGroupBox;
            edModelFileName: TEdit;
            btBrowse: TButton;
            gbSelectTeam: TGroupBox;
            rbDefault: TRadioButton;
            rbRed: TRadioButton;
            rbBlue: TRadioButton;
            paButtons: TPanel;
            btOK: TButton;
            btCancel: TButton;
            btQuit: TButton;
            tiDrawPreview: TTimer;
            odOpenDialog: TOpenDialog;

            procedure FormCreate(pSender: TObject);
            procedure rgCacheOptionsClick(pSender: TObject);
            procedure btQuitClick(pSender: TObject);
            procedure btCancelClick(pSender: TObject);
            procedure btOKClick(pSender: TObject);
            procedure btBrowseClick(pSender: TObject);
            procedure tiDrawPreviewTimer(pSender: TObject);
            procedure OnSelectTeam(pSender: TObject);

        private type
            ITextures = TList<Vcl.Graphics.TBitmap>;

        private
            m_pMD3:          TQRMD3Group;
            m_pTextures:     ITextures;
            m_Team:          EQRMD3PackageTeam;
            m_ModelRendered: Boolean;
            m_Closing:       Boolean;

            {**
             Resets interface to default values
            }
            procedure Reset;

            {**
             Loads preview image
             @param(pStream @bold([in, out]) Model stream for which preview should be shown)
             @return(@true on success, otherwise @false)
            }
            function LoadPreview(var pStream: TStream): Boolean;

            {**
             Called when mesh texture should be loaded
             @param(pGroup Group at which model belongs)
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
            }
            procedure OnDrawCustomModelItem(const pGroup: TQRModelGroup;
                                                  pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4;
                                        index, nextIndex: NativeInt;
                               const interpolationFactor: Double);

        protected
            {**
             Options form message loop
             @param(message Message sent by Windows)
            }
            procedure WndProc(var message: TMessage); override;

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

            {**
            * Gets selected team
            *@return selected team
            *}
            function GetSelectedTeam: EQRMD3PackageTeam; virtual;

            {**
             Checks if application is closing
             @return(@true if application is closing, otherwise @false)
            }
            function IsAppClosing: Boolean; virtual;
    end;

var
    Options: TOptions;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
// TOptions
//--------------------------------------------------------------------------------------------------
constructor TOptions.Create(pOwner: TComponent);
var
    hPackageInstance: THandle;
    pModelStream:     TResourceStream;
begin
    inherited Create(pOwner);

    m_pTextures     := ITextures.Create;
    m_pMD3          := nil;
    m_Team          := EQR_PT_MD3_Default;
    m_ModelRendered := False;
    m_Closing       := False;

    // get module instance at which this form belongs
    hPackageInstance := FindClassHInstance(TOptions);

    // found it?
    if (hPackageInstance <> 0) then
    begin
        pModelStream := nil;

        try
            // load resources
            if (FindResource(hPackageInstance, PChar('ID_MD3_MODEL'), RT_RCDATA) <> 0)
            then
                pModelStream := TResourceStream.Create(hPackageInstance,
                                                       PChar('ID_MD3_MODEL'),
                                                       RT_RCDATA);

            // load preview
            LoadPreview(TStream(pModelStream));
        finally
            pModelStream.Free;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
destructor TOptions.Destroy;
var
    i: NativeUInt;
begin
    // delete all textures
    if (m_pTextures.Count > 0) then
        for i := 0 to m_pTextures.Count - 1 do
            m_pTextures[i].Free;

    // delete texture list
    m_pTextures.Free;

    // delete model
    m_pMD3.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.FormCreate(pSender: TObject);
begin
    // to see form in taskbar even if main form is still not created
    SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.rgCacheOptionsClick(pSender: TObject);
begin
    // enable advanced cache options only if "create cache" option is selected
    ckShowCollisions.Enabled := (rgCacheOptions.ItemIndex <> 1);
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.btQuitClick(pSender: TObject);
begin
    m_Closing := True;
    Application.Terminate;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.btCancelClick(pSender: TObject);
begin
    Reset;
    Close;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.btOKClick(pSender: TObject);
begin
    Close;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.btBrowseClick(pSender: TObject);
var
    pStatus:     TQRModelJobStatus;
    pFileStream: TFileStream;
begin
    // previous MD3 model was already loaded?
    if (Assigned(m_pMD3)) then
    begin
        // query job status
        pStatus := m_pMD3.QueryJobStatus;

        // was previous model fully loaded and drawn?
        if (Assigned(pStatus) and (pStatus.Status <> EQR_JS_Done) and (pStatus.Status <> EQR_JS_Error)) then
            Exit;
    end;

    // clear previous interface
    edModelFileName.Text    := '';
    odOpenDialog.InitialDir := ExtractFilePath(Application.ExeName);

    // show open file dialog to user and check if dialog was canceled
    if (not odOpenDialog.Execute) then
        Exit;

    // file exists?
    if (not FileExists(odOpenDialog.FileName)) then
        Exit;

    // show selected file name
    edModelFileName.Text := odOpenDialog.FileName;

    pFileStream := nil;

    try
        // open package stream
        pFileStream := TFileStream.Create(edModelFileName.Text, fmOpenRead);

        // load model preview
        if (not LoadPreview(TStream(pFileStream))) then
        begin
            m_Team               := EQR_PT_MD3_Default;
            rbDefault.Checked    := True;
            edModelFileName.Text := '';
        end;
    finally
        pFileStream.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.tiDrawPreviewTimer(pSender: TObject);
var
    pStatus:                       TQRModelJobStatus;
    pOverlayForm:                  IQRSmartPointer<TForm>;
    pOverlay, pAntialiasedOverlay: Vcl.Graphics.TBitmap;
    width, height:                 Integer;
    hDC, hRC:                      THandle;
begin
    // model already rendered?
    if (m_ModelRendered) then
    begin
        tiDrawPreview.Enabled := False;

        // delete model
        if (Assigned(m_pMD3)) then
        begin
            m_pMD3.Free;
            m_pMD3 := nil;
        end;

        Exit;
    end;

    pStatus := m_pMD3.QueryJobStatus;

    if (Assigned(pStatus) and (pStatus.Status = EQR_JS_Error)) then
    begin
        m_ModelRendered       := True;
        tiDrawPreview.Enabled := False;

        MessageDlg('Failed to open model.', mtError, [mbOK], 0);

        // delete model
        if (Assigned(m_pMD3)) then
        begin
            m_pMD3.Free;
            m_pMD3 := nil;
        end;

        Exit;
    end;

    if ((not Assigned(pStatus)) or (pStatus.Status <> EQR_JS_Done)) then
        Exit;

    // calculate rendering surface width and height (4x higher to allow 4x4 antialiasing to be
    // applied later)
    width  := imPreview.Width  * 4;
    height := imPreview.Height * 4;

    // create overlay render surface (cannot use a bitmap directly, unfortunately)
    pOverlayForm              := TQRSmartPointer<TForm>.Create(TForm.Create(nil));
    pOverlayForm.ClientWidth  := width;
    pOverlayForm.ClientHeight := height;
    pOverlayForm.Visible      := False;

    // initialize OpenGL
    if (not TQROpenGLHelper.EnableOpenGL(pOverlayForm.Handle, hDC, hRC)) then
        Exit;

    pOverlay            := nil;
    pAntialiasedOverlay := nil;

    try
        // configure OpenGL
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_FRONT);
        glEnable(GL_TEXTURE_2D);

        // create viewport
        TQROpenGLHelper.CreateViewport(width, height, true);

        // clear scene
        glClearColor(0.0, 0.0, 0.0, 1.0);
        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

        // draw model
        m_pMD3.Draw(0.0);

        // create image overlay
        pOverlay := Vcl.Graphics.TBitmap.Create;
        TQROpenGLHelper.GetBitmapFromOpenGL(pOverlay);

        // create antialiased final image overlay
        pAntialiasedOverlay := Vcl.Graphics.TBitmap.Create;

        // apply 4x4 antialiasing on the rendered image
        TQRGDIHelper.ApplyAntialiasing(pOverlay, pAntialiasedOverlay, 4);

        // show final image
        imPreview.Picture.Assign(pAntialiasedOverlay);
    finally
        pAntialiasedOverlay.Free;
        pOverlay.Free;

        // shutdown OpenGL
        TQROpenGLHelper.DisableOpenGL(pOverlayForm.Handle, hDC, hRC);
    end;

    m_ModelRendered := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.OnSelectTeam(pSender: TObject);
var
    selectedTeam, prevTeam: EQRMD3PackageTeam;
    pStream:                TStream;
    hPackageInstance:       THandle;
begin
    selectedTeam := EQR_PT_MD3_Default;

    // get newly selected team
    if (rbDefault.Checked) then
        selectedTeam := EQR_PT_MD3_Default
    else
    if (rbRed.Checked) then
        selectedTeam := EQR_PT_MD3_Red
    else
    if (rbBlue.Checked) then
        selectedTeam := EQR_PT_MD3_Blue;

    // nothing to do?
    if (m_Team = selectedTeam) then
        Exit;

    // change team
    prevTeam := m_Team;
    m_Team   := selectedTeam;

    // get module instance at which this form belongs
    hPackageInstance := FindClassHInstance(TOptions);

    // found it?
    if (hPackageInstance = 0) then
        Exit;

    pStream := nil;

    try
        // open package stream
        if (Length(edModelFileName.Text) = 0) then
        begin
            // load resources
            if (FindResource(hPackageInstance, PChar('ID_MD3_MODEL'), RT_RCDATA) <> 0) then
                pStream := TResourceStream.Create(hPackageInstance,
                                                  PChar('ID_MD3_MODEL'),
                                                  RT_RCDATA);
        end
        else
            pStream := TFileStream.Create(edModelFileName.Text, fmOpenRead);

        // load model preview
        if (not LoadPreview(pStream)) then
        begin
            // restore previous team
            m_Team := prevTeam;

            // restore previous selection
            case (m_Team) of
                EQR_PT_MD3_Default: rbDefault.Checked := True;
                EQR_PT_MD3_Red:     rbRed.Checked     := True;
                EQR_PT_MD3_Blue:    rbBlue.Checked    := True;
            end;
        end;
    finally
        pStream.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.Reset;
begin
    // reset interface to default values
    rgCacheOptions.ItemIndex := 0;
    ckFullScreen.Checked     := False;
    ckShowCollisions.Checked := False;
    ckUseShader.Checked      := True;
end;
//--------------------------------------------------------------------------------------------------
function TOptions.LoadPreview(var pStream: TStream): Boolean;
var
    i:                  NativeUInt;
    pMD3:               TQRMD3Group;
    pColor:             TQRColor;
    modelOptions:       TQRModelOptions;
    framedModelOptions: TQRFramedModelOptions;
begin
    // clear previous interface
    tiDrawPreview.Enabled := False;
    imPreview.Picture.Assign(nil);

    // delete previous textures
    if (m_pTextures.Count > 0) then
        for i := 0 to m_pTextures.Count - 1 do
            m_pTextures[i].Free;

    m_pTextures.Clear;

    // delete previous model
    if (Assigned(m_pMD3)) then
    begin
        m_pMD3.Free;
        m_pMD3 := nil;
    end;

    // no stream to load?
    if (not Assigned(pStream)) then
    begin
        Result := False;
        Exit;
    end;

    pColor := nil;
    pMD3   := nil;

    try
        // create MD3 model, populate callbacks
        pMD3                   := TQRMD3Group.Create;
        pMD3.OnLoadMeshTexture := OnLoadMeshTexture;
        pMD3.OnCustomDrawItem  := OnDrawCustomModelItem;

        // configure model options
        modelOptions := [EQR_MO_Without_Normals];

        pColor := TQRColor.Create(255, 255, 255, 255);

        if (not pMD3.Load(pStream,
                          pColor,
                          false,
                          modelOptions,
                          framedModelOptions,
                          m_Team))
        then
        begin
            pStream := nil;
            Result  := False;
            Exit;
        end;

        pStream := nil;

        // set default animation gesture
        pMD3.SetAnimation('upper', EQR_AG_MD3_Torso_Stand);
        pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Walk);

        // place model into 3D world
        pMD3.Translation^ := TQRVector3D.Create(0.0, -5.0, -100.0);
        pMD3.RotationX    := -PI / 2.0; // -90°
        pMD3.RotationY    := -PI / 4.0; // -45°

        m_pMD3 := pMD3;
        pMD3   := nil;

        m_ModelRendered       := False;
        tiDrawPreview.Enabled := True;
    finally
        pColor.Free;
        pMD3.Free;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TOptions.OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                    const pModel: TQRModel;
                                         pBitmap: Vcl.Graphics.TBitmap;
                                        pTexture: TQRTexture;
                                    out loadNext: Boolean): Boolean;
var
    pModelTexture: Vcl.Graphics.TBitmap;
begin
    // OpenGL is still not initialized when textures are loaded, this is why it's not possible to
    // link them here. To workaround that, textures are stored inside a set of bitmaps, and their
    // addresses are used as a key to retrieve them later, when required
    if (Assigned(pBitmap)) then
    begin
        pModelTexture := nil;

        try
            pModelTexture       := Vcl.Graphics.TBitmap.Create;
            pModelTexture.Assign(pBitmap);
            m_pTextures.Add(pModelTexture);
            pTexture.CustomData := pModelTexture;
            pModelTexture       := nil;
        finally
            pModelTexture.Free;
        end;
    end
    else
        pTexture.Enabled := False;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.OnDrawCustomModelItem(const pGroup: TQRModelGroup;
                                               pModel: TQRModel;
                                       const textures: TQRTextures;
                                         const matrix: TQRMatrix4x4;
                                     index, nextIndex: NativeInt;
                            const interpolationFactor: Double);
var
    pMD3Model:                 TQRMD3Model;
    mesh:                      TQRMesh;
    textureCount, i:           NativeUInt;
    modelTextures:             TQRTextures;
    textureIndex, pixelFormat: Integer;
    texIndexGL:                GLint;
    pBitmap:                   IQRSmartPointer<Vcl.Graphics.TBitmap>;
    pPixels:                   PByte;
begin
    // no model to draw?
    if (not Assigned(pModel)) then
        Exit;

    // get MD3 model
    pMD3Model := TQRMD3Model(pModel);

    // found it?
    if (not Assigned(pMD3Model)) then
        Exit;

    // get mesh to draw
    pMD3Model.GetMesh(index, mesh, nil);

    // no mesh to draw?
    if (Length(mesh) = 0) then
        Exit;

    textureCount := Length(textures);

    try
        SetLength(modelTextures, textureCount);

        // iterate through textures to link
        if (textureCount > 0) then
            for i := 0 to textureCount - 1 do
            begin
                modelTextures[i] := nil;

                // is texture enabled?
                if (not textures[i].Enabled) then
                    continue;

                // get previoulsy loaded texture to link in set
                textureIndex := m_pTextures.IndexOf(textures[i].CustomData);

                // found it?
                if (textureIndex >= 0) then
                begin
                    pBitmap := TQRSmartPointer<Vcl.Graphics.TBitmap>.Create();

                    // make sure texture is a power of 2 texture (OpenGL may not support non POT textures)
                    TQRModelGroupHelper.MakeTexturePowerOf2(m_pTextures[textureIndex], pBitmap);

                    if (pBitmap.PixelFormat = pf32bit) then
                        pixelFormat := GL_RGBA
                    else
                        pixelFormat := GL_RGB;

                    pPixels := nil;

                    try
                        // convert texture bitmap to pixel array, and create OpenGL texture from array
                        TQROpenGLHelper.BytesFromBitmap(pBitmap, pPixels, false, false);
                        texIndexGL := TQROpenGLHelper.CreateTexture(pBitmap.Width,
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

                    // set textures to use
                    modelTextures[i]       := TQRTexture.Create;
                    modelTextures[i].Name  := textures[i].Name;
                    modelTextures[i].Index := texIndexGL;
                end;
            end;

        // draw mesh
        TQROpenGLHelper.Draw(mesh, matrix, modelTextures);
    finally
        // clear memory
        if (textureCount > 0) then
            for i := 0 to textureCount - 1 do
                if (Assigned(modelTextures[i])) then
                    modelTextures[i].Free;
    end;

    glFlush;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.WndProc(var message: TMessage);
begin
    // dispatch message
    case (message.Msg) of
        WM_SYSCOMMAND:
        begin
            // close button was clicked on form?
            if (message.WParam = SC_CLOSE) then
            begin
                m_Closing := True;

                // really close the application
                Application.Terminate;
            end;
        end;
    end;

    inherited WndProc(message);
end;
//--------------------------------------------------------------------------------------------------
function TOptions.GetSelectedTeam: EQRMD3PackageTeam;
begin
    Result := m_Team;
end;
//--------------------------------------------------------------------------------------------------
function TOptions.IsAppClosing: Boolean;
begin
    Result := m_Closing;
end;
//--------------------------------------------------------------------------------------------------

end.
