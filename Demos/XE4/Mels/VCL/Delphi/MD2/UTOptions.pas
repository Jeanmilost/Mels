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
     System.Variants,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.ExtCtrls,
     Vcl.StdCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Winapi.Messages,
     Winapi.Windows,
     UTQR3D,
     UTQRGeometry,
     UTQRGraphics,
     UTQRFiles,
     UTQRModel,
     UTQRModelGroup,
     UTQRMD2,
     UTQRMD2ModelGroup,
     UTQRVCLHelpers,
     UTQROpenGLHelper,
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
    TOptions = class(TForm)
        published
            paPreview: TPanel;
            imPreview: TImage;
            paMain: TPanel;
            gbLoadOptions: TGroupBox;
            ckShowDefaultFrame: TCheckBox;
            ckRunGestureWhenReady: TCheckBox;
            gbRenderOptions: TGroupBox;
            ckShowCollisions: TCheckBox;
            ckUseShader: TCheckBox;
            ckFullScreen: TCheckBox;
            ckPreCalculateLight: TCheckBox;
            ckUseOrthoMatrix: TCheckBox;
            rgCacheOptions: TRadioGroup;
            paButtons: TPanel;
            btOK: TButton;
            btCancel: TButton;
            btQuit: TButton;
            tiDrawPreview: TTimer;

            procedure FormCreate(pSender: TObject);
            procedure rgCacheOptionsClick(pSender: TObject);
            procedure btQuitClick(pSender: TObject);
            procedure btCancelClick(pSender: TObject);
            procedure btOKClick(pSender: TObject);
            procedure tiDrawPreviewTimer(pSender: TObject);

        private
            m_pMD2:          TQRMD2Group;
            m_ModelRendered: Boolean;
            m_Closing:       Boolean;

            {**
             Resets interface to default values
            }
            procedure Reset;

            {**
             Loads preview image
            }
            procedure LoadPreview;

            {**
             Called when texture should be loaded
             @param(pGroup Group at which model belongs)
             @param(pModel Model for which texture is required)
             @param(pBitmap Whenever possible, the bitmap containing the texture, @nil if not available)
             @param(pTexture @bold([in, out]) Texture info, contains loaded index when function ends)
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
            procedure WndProc(var message: TMessage); virtual;

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
begin
    inherited Create(pOwner);

    m_pMD2          := nil;
    m_ModelRendered := False;
    m_Closing       := False;

    LoadPreview;
end;
//--------------------------------------------------------------------------------------------------
destructor TOptions.Destroy;
begin
    m_pMD2.Free;

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
    gbLoadOptions.Enabled         := (rgCacheOptions.ItemIndex = 0);
    ckShowDefaultFrame.Enabled    :=  gbLoadOptions.Enabled;
    ckRunGestureWhenReady.Enabled :=  gbLoadOptions.Enabled;
    ckShowCollisions.Enabled      := (rgCacheOptions.ItemIndex <> 1);
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
procedure TOptions.tiDrawPreviewTimer(pSender: TObject);
begin
    // model already rendered?
    if (m_ModelRendered) then
    begin
        tiDrawPreview.Enabled := False;

        // delete model
        m_pMD2.Free;
        m_pMD2 := nil;

        Exit;
    end;

    // draw model
    m_pMD2.Draw(0.0);
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.Reset;
begin
    // reset interface to default values
    rgCacheOptions.ItemIndex      := 0;
    ckShowDefaultFrame.Checked    := True;
    ckRunGestureWhenReady.Checked := False;
    ckFullScreen.Checked          := False;
    ckShowCollisions.Checked      := False;
    ckUseShader.Checked           := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.LoadPreview;
var
    pMD2:               TQRMD2Group;
    hPackageInstance:   THandle;
    pModelStream:       TResourceStream;
    pMemDir:            TQRMemoryDir;
    pColor:             TQRColor;
    modelOptions:       TQRModelOptions;
    framedModelOptions: TQRFramedModelOptions;
begin
    pMD2         := nil;
    pColor       := nil;
    pMemDir      := nil;
    pModelStream := nil;

    try
        // create MD2 model, populate callbacks
        pMD2                   := TQRMD2Group.Create;
        pMD2.OnLoadMeshTexture := OnLoadMeshTexture;
        pMD2.OnCustomDrawItem  := OnDrawCustomModelItem;

        // configure model options
        modelOptions := [EQR_MO_Without_Normals];

        // get module instance at which this form belongs
        hPackageInstance := FindClassHInstance(TOptions);

        // found it?
        if (hPackageInstance = 0) then
            Exit;

        // load resources
        if (FindResource(hPackageInstance, PChar('ID_MD2_MODEL'), RT_RCDATA) <> 0)
        then
            pModelStream := TResourceStream.Create(hPackageInstance,
                                                   PChar('ID_MD2_MODEL'),
                                                   RT_RCDATA);

        // create in-memory model directory
        pMemDir := TQRMemoryDir.Create(True);

        if (not pMemDir.AddFile('marvin.md2', pModelStream, False)) then
            Exit;

        pModelStream := nil;

        // place model into 3D world
        pMD2.Translation^ :=   TQRVector3D.Create(0.0, 0.0, -100.0);
        pMD2.RotationX    := -(PI / 2.0); // -90°
        pMD2.RotationZ    := -(PI / 4.0); // -45°

        // set gesture to run
        pMD2.Gesture := 0;

        pColor := TQRColor.Create(255, 255, 255, 255);

        // load model
        if (not pMD2.Load(pMemDir,
                          'marvin',
                          pColor,
                          nil,
                          false,
                          modelOptions,
                          framedModelOptions))
        then
            Exit;

        m_pMD2  := pMD2;
        pMemDir := nil;
        pMD2    := nil;
    finally
        pModelStream.Free;
        pMemDir.Free;
        pColor.Free;
        pMD2.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TOptions.OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                    const pModel: TQRModel;
                                         pBitmap: Vcl.Graphics.TBitmap;
                                        pTexture: TQRTexture;
                                    out loadNext: Boolean): Boolean;
begin
    // not used here, as the rendering is done once to a bitmap inside the OnDrawCustomModelItem
    // function. As OpenGl is still not initialized when this function is called, the texture also
    // cannot be prepared for now
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
    width, height, pixelFormat:             Integer;
    pOverlayForm:                           TForm;
    hDC, hRC, hPackageInstance:             THandle;
    pTextureStream:                         TResourceStream;
    pBitmap, pOverlay, pAntialiasedOverlay: Vcl.Graphics.TBitmap;
    pMD2Model:                              TQRMD2Model;
    mesh:                                   TQRMesh;
    pLoadedTexture:                         TQRTexture;
    loadedTextures:                         TQRTextures;
    pPixels:                                PByte;
    textureIndex:                           GLint;
begin
    // no model to draw?
    if (not Assigned(pModel)) then
        Exit;

    // get MD2 model
    pMD2Model := TQRMD2Model(pModel);

    // found it?
    if (not Assigned(pMD2Model)) then
        Exit;

    // calculate rendering surface width and height (4x higher to allow 4x4 antialiasing to be
    // applied later)
    width  := imPreview.Width  * 4;
    height := imPreview.Height * 4;

    // create overlay render surface (cannot use a bitmap directly, unfortunately)
    pOverlayForm              := TForm.Create(nil);
    pOverlayForm.ClientWidth  := width;
    pOverlayForm.ClientHeight := height;
    pOverlayForm.Visible      := False;

    // initialize OpenGL
    if (not TQROpenGLHelper.EnableOpenGL(pOverlayForm.Handle, hDC, hRC)) then
        Exit;

    try
        // configure OpenGL
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_FRONT);
        glEnable(GL_TEXTURE_2D);

        // create viewport
        TQROpenGLHelper.CreateViewport(width, height, true);

        // get model first mesh
        pMD2Model.GetMesh(0, mesh, nil);

        // get module instance at which this form belongs
        hPackageInstance := FindClassHInstance(TOptions);

        // found it?
        if (hPackageInstance = 0) then
            Exit;

        // get texture from stream
        if (FindResource(hPackageInstance, PChar('ID_MD2_TEXTURE'), RT_RCDATA) <> 0)
        then
            pTextureStream := TResourceStream.Create(hPackageInstance,
                                                     PChar('ID_MD2_TEXTURE'),
                                                     RT_RCDATA);

        // load MD2 texture
        pBitmap := Vcl.Graphics.TBitmap.Create;
        pBitmap.LoadFromStream(pTextureStream);

        if (pBitmap.PixelFormat = pf32bit) then
            pixelFormat := GL_RGBA
        else
            pixelFormat := GL_RGB;

        pPixels := nil;

        try
            // convert texture bitmap to pixel array, and create OpenGL texture from array
            TQROpenGLHelper.BytesFromBitmap(pBitmap, pPixels, false, false);
            textureIndex := TQROpenGLHelper.CreateTexture(pBitmap.Width,
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

        // create model texture
        SetLength(loadedTextures, 1);
        loadedTextures[0]       := TQRTexture.Create;
        loadedTextures[0].Index := textureIndex;
        loadedTextures[0].Name  := mesh[0].m_Name;

        // clear scene
        glClearColor(0.0, 0.0, 0.0, 1.0);
        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

        // draw model first mesh
        TQROpenGLHelper.Draw(mesh, matrix, loadedTextures);

        glFlush;

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
        if (Length(loadedTextures) <> 0) then
        begin
            loadedTextures[0].Free;
            SetLength(loadedTextures, 0);
        end;

        pTextureStream.Free;
        pAntialiasedOverlay.Free;
        pOverlay.Free;
        pBitmap.Free;

        SetLength(mesh, 0);

        // shutdown OpenGL
        TQROpenGLHelper.DisableOpenGL(pOverlayForm.Handle, hDC, hRC);
    end;

    m_ModelRendered := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.WndProc(var message: TMessage);
begin
    // dispatch message
    case (message.Msg) of
        WM_SYSCOMMAND:
            // close button was clicked on form?
            if (message.WParam = SC_CLOSE) then
                // really close the application
                Application.Terminate;
    end;

    inherited WndProc(message);
end;
//--------------------------------------------------------------------------------------------------
function TOptions.IsAppClosing: Boolean;
begin
    Result := m_Closing;
end;
//--------------------------------------------------------------------------------------------------

end.
