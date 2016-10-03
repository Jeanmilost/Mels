{**************************************************************************************************
 * ==> UTQRVCLModelComponentGL -------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides a basic model component using the VCL and OpenGL to draw it *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLModelComponentGL;

interface
    // do not include XE7.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE XE7.OpenGLext *)

    // resources
    {$R UTQRVCLModelComponentGL.res}

uses System.Classes,
     System.SysUtils,
     UTQRDesignPatterns,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRModel,
     UTQRModelGroup,
     UTQRLogging,
     UTQRVCLAnimationTimer,
     UTQRVCLModelRendererGL,
     UTQRVCLModelShaderGL,
     UTQRVCLModelRenderSurfaceGL,
     UTQRVCLModelComponentPropertiesGL,
     UTQRVCLHelpersGL,
     UTQRDesignerHook,       UTQRVCLHelpers,
     Vcl.Graphics,
     Vcl.Imaging.pngimage,
     Vcl.Controls,
     Vcl.ExtCtrls,
     Vcl.Forms,
     Vcl.AppEvnts,
     Winapi.Windows,
     Winapi.Messages,
     // unfortunately the required OpenGL headers does not exist or are incomplete in XE4 and
     // earlier, so the DelphiGL component (provided with installation) should be used instead
     XE7.OpenGL, XE7.OpenGLext;

type
    {**
    * Antialiasing mode
    *}
    EQRAntialiasingMode =
    (
        EQR_AM_None    = 0,
        EQR_AM_FSAA_2x,
        EQR_AM_FSAA_4x,
        EQR_AM_FSAA_8x
    );

    {**
    * Called when OpenGL was created and should be configured
    *@param pSender - event sender
    *@param hDC - device context
    *@param hGLRC - OpenGL render context
    *@param pRenderer - OpenGL renderer
    *@param pShader - OpenGL shader
    *}
    TQRConfigureOpenGL = procedure(pSender: TObject;
                                hDC, hGLRC: THandle;
                                 pRenderer: TQRVCLModelRendererGL;
                                   pShader: TQRVCLModelShaderGL) of object;

    {**
    * Called when projection and view matrix are created
    *@param pSender - event sender
    *@param[in, out] projectionMatrix - projection matrix to use
    *@param[in, out] viewMatrix - view matrix to use
    *@return true to use user defined matrix instead of default, otherwise false
    *}
    TQRCreateSceneMatrix = function(pSender: TObject;
                       var projectionMatrix,
                                 viewMatrix: TQRMatrix4x4): Boolean of object;

    {**
    * Called when a scene is initialized, before OpenGL clears the render target
    *@param pSender - event sender
    *@param hDC - device context
    *}
    TQRInitializeSceneEvent = procedure(pSender: TObject; hDC: THandle) of object;

    {**
    * Called before a scene is drawn
    *@param pSender - event sender
    *@param hDC - device context
    *@param hGLRC - OpenGL render context
    *@param pRenderer - OpenGL renderer
    *@param pShader - OpenGL shader
    *}
    TQRBeforeDrawSceneEvent = procedure(pSender: TObject;
                                     hDC, hGLRC: THandle;
                                      pRenderer: TQRVCLModelRendererGL;
                                        pShader: TQRVCLModelShaderGL) of object;

    {**
    * Called after a scene is drawn
    *@param pSender - event sender
    *@param hDC - device context
    *@param hGLRC - OpenGL render context
    *@param pRenderer - OpenGL renderer
    *@param pShader - OpenGL shader
    *}
    TQRAfterDrawSceneEvent = procedure(pSender: TObject;
                                    hDC, hGLRC: THandle;
                                     pRenderer: TQRVCLModelRendererGL;
                                       pShader: TQRVCLModelShaderGL) of object;

    {**
    * Called when a scene is finalized, after OpenGL has swapped the render target
    *@param pSender - event sender
    *@param hDC - device context
    *}
    TQRFinalizeSceneEvent = procedure(pSender: TObject; hDC: THandle) of object;

    {**
    * Called when collisions should be detected on the model
    *@param pSender - event sender
    *@param modelMatrix - model matrix
    *@param pAABBTree - model aligned-axis bounding box tree
    *}
    TQRDetectCollisionsEvent = procedure(pSender: TObject;
                               const modelMatrix: TQRMatrix4x4;
                                       pAABBTree: TQRAABBTree) of object;

    {**
    * Basic model component using the VCL and OpenGL to draw it
    *}
    TQRVCLModelComponentGL = class(TWinControl, IQRObserver)
        private
            // the allowed property is a little special, that's why it is not accessible as the
            // others. Its purpose is to determine whether OpenGL was correctly initialized, and if
            // the control may access to all the OpenGL function it requires to perform the drawing
            m_Allowed:       Boolean;
            m_pDefaultImage: TPngImage;

        protected
            m_pColor:               TQRVCLModelComponentColorGL;
            m_pAlphaBlending:       TQRVCLModelComponentAlphaBlendingPropertyGL;
            m_pRenderer:            TQRVCLModelRendererGL;
            m_pShader:              TQRVCLModelShaderGL;
            m_pRenderSurface:       TQRVCLModelRenderSurfaceGL;
            m_pOverlay:             Vcl.Graphics.TBitmap;
            m_pAntialiasingOverlay: Vcl.Graphics.TBitmap;
            m_hBackgroundBrush:     HBRUSH;
            m_ViewMatrix:           TQRMatrix4x4;
            m_ProjectionMatrix:     TQRMatrix4x4;
            m_AntialiasingMode:     EQRAntialiasingMode;
            m_UseShader:            Boolean;
            m_SupportsGDI:          Boolean;
            m_LogMessageLoop:       Boolean;
            m_fOnConfigureOpenGL:   TQRConfigureOpenGL;
            m_fOnLoadTexture:       TQRLoadMeshTextureEvent;
            m_fOnCreateSceneMatrix: TQRCreateSceneMatrix;
            m_fOnInitializeScene:   TQRInitializeSceneEvent;
            m_fOnBeforeDrawScene:   TQRBeforeDrawSceneEvent;
            m_fOnAfterDrawScene:    TQRAfterDrawSceneEvent;
            m_fOnFinalizeScene:     TQRFinalizeSceneEvent;
            m_fOnDetectCollisions:  TQRDetectCollisionsEvent;

            {**
            * Component Windows procedure
            *@param message - Windows message
            *}
            procedure WndProc(var message: TMessage); override;

            {**
            * Sets antialiasing mode
            *@param mode - antialiasing mode
            *}
            procedure SetAntialiasingMode(mode: EQRAntialiasingMode); virtual;

            {**
            * Gets antialiasing factor
            *@return antialiasing factor to apply
            *}
            function GetAntialiasingFactor: NativeInt; virtual;

            {**
            * Called after all control properties were loaded from DFM files
            *}
            procedure Loaded; override;

            {**
            * Called when control is resized
            *}
            procedure Resize; override;

            {**
            * Creates the control parameters
            *@param params - control parameters
            *}
            procedure CreateParams(var params: TCreateParams); override;

            {**
            * Creates the component Windows handle
            *@param params - Windows parameters used to create handle
            *}
            procedure CreateWindowHandle(const params: TCreateParams); override;

            {**
            * Deletes the component Windows handle
            *}
            procedure DestroyWindowHandle; override;

            {**
            * Releases the draw context
            *}
            procedure ReleaseDrawContext; virtual;

            {**
            * Creates a viewport for the component
            *@param width - viewport width
            *@param height - viewport height
            *}
            procedure CreateViewport(width, height: NativeUInt); virtual;

            {**
            * Builds shader
            *@param pVertexPrg - vertex shader program to compile and build
            *@param pFragmentPrg - fragment shader program to compile and build
            *@return true on success, otherwise false
            *}
            function BuildShader(pVertexPrg, pFragmentPrg: TStream): Boolean; virtual;

            {**
            * Draws a default image, in case a problem occurred while OpenGL library was initialized
            *}
            function DrawDefaultImage(hDC: THandle): Boolean; virtual;

            {**
            * Draws the scene
            *@param hDC - internal control device context that OpenGL should use to draw the scene
            *}
            procedure DrawScene(hDC: THandle); virtual;

            {**
            * Draws the scene to a device context
            *@param message - Windows message containing device context to draw to
            *@return true if message was processed, otherwise false
            *}
            function DrawSceneTo(var message: TMessage): Boolean; overload; virtual;

            {**
            * Draws the scene to a device context
            *@param hDC - device context to draw the scene to
            *@param x - position on the x axis where the scene will be drawn, in pixels
            *@param y - position on the y axis where the scene will be drawn, in pixels
            *}
            procedure DrawSceneTo(hDC: THandle; x, y: Integer); overload; virtual;

            {**
            * Called when OpenGL can be configured
            *}
            procedure OnConfigOpenGL; virtual; abstract;

            {**
            * Called when the scene content should be drawn
            *@param hDC - internal control device context that OpenGL should use to draw the scene
            *}
            procedure OnDrawSceneContent(hDC: THandle); virtual; abstract;

            {**
            * Receives and processes important messages from properties
            *@param pSender - event sender
            *@param message - message to send to owner
            *}
            function OnReceivePropNotification(pSender: TObject;
                                               message: EQRPropMessages): Boolean; virtual;

            {**
            * Called when subject send a notification to the observer
            *@param message - notification message
            *}
            procedure OnNotified(message: TQRMessage); virtual;

        public
            {**
            * Constructor
            *@param pOwner - component owner
            *}
            constructor Create(pOwner: TComponent); override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Paints the control content to a device context
            *@param dc - device context to paint to
            *@param x - position on the x axis where the scene will be drawn on the context, in pixels
            *@param y - position on the y axis where the scene will be drawn on the context, in pixels
            *}
            procedure PaintTo(dc: HDC; x, y: Integer); overload;

            {**
            * Copies the property attributes from another property
            *@param pSource - source property to copy from
            *}
            procedure Assign(pSource: TPersistent); override;

            { Public properties }
            property Allowed: Boolean read m_Allowed;
            {$IFDEF DEBUG}
                property LogMessageLoop: Boolean read m_LogMessageLoop write m_LogMessageLoop stored False;
            {$ENDIF}

        published
            { Published properties }
            property Color:               TQRVCLModelComponentColorGL                 read m_pColor               write m_pColor;
            property AlphaBlending:       TQRVCLModelComponentAlphaBlendingPropertyGL read m_pAlphaBlending       write m_pAlphaBlending;
            property Antialiasing:        EQRAntialiasingMode                         read m_AntialiasingMode     write SetAntialiasingMode    default EQR_AM_None;
            property OnConfigureOpenGL:   TQRConfigureOpenGL                          read m_fOnConfigureOpenGL   write m_fOnConfigureOpenGL;
            property OnLoadTexture:       TQRLoadMeshTextureEvent                     read m_fOnLoadTexture       write m_fOnLoadTexture;
            property OnCreateSceneMatrix: TQRCreateSceneMatrix                        read m_fOnCreateSceneMatrix write m_fOnCreateSceneMatrix;
            property OnInitializeScene:   TQRInitializeSceneEvent                     read m_fOnInitializeScene   write m_fOnInitializeScene;
            property OnBeforeDrawScene:   TQRBeforeDrawSceneEvent                     read m_fOnBeforeDrawScene   write m_fOnBeforeDrawScene;
            property OnAfterDrawScene:    TQRAfterDrawSceneEvent                      read m_fOnAfterDrawScene    write m_fOnAfterDrawScene;
            property OnFinalizeScene:     TQRFinalizeSceneEvent                       read m_fOnFinalizeScene     write m_fOnFinalizeScene;
            property OnDetectCollisions:  TQRDetectCollisionsEvent                    read m_fOnDetectCollisions  write m_fOnDetectCollisions;
            property Align;
            property Anchors;
            property Constraints;
            property Enabled;
    end;

    {**
    * Called when a static model item should be drawn on the scene
    *@param pSender - event sender
    *@param hDC - device context
    *@param hGLRC - OpenGL render context
    *@param pRenderer - OpenGL renderer
    *@param pShader - OpenGL shader
    *@param pGroup - group at which model belongs
    *@param pModel - model to draw
    *@param textures - textures belonging to model, in the order where they should be combined
    *@param matrix - model matrix
    *@param pMesh - mesh to draw, can be nil (depends on selected options)
    *@param pAABBTree - aligned-axis bounding box tree matching with mesh, can be nil (depends on selected options)
    *@return true if framed model item was drawn on the scene, otherwise false
    *}
    TQRDrawSceneStaticModelItemEvent = function (pSender: TObject;
                                              hDC, hGLRC: THandle;
                                               pRenderer: TQRVCLModelRendererGL;
                                                 pShader: TQRVCLModelShaderGL;
                                            const pGroup: TQRModelGroup;
                                            const pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4;
                                             const pMesh: PQRMesh;
                                         const pAABBTree: TQRAABBTree): Boolean of object;

    {**
    * Basic model component for static models and using the VCL and OpenGL to draw it
    *}
    TQRVCLStaticModelComponentGL = class(TQRVCLModelComponentGL)
        protected
            m_fDrawSceneStaticModelItemEvent: TQRDrawSceneStaticModelItemEvent;

            {**
            * Called when OpenGL can be configured
            *}
            procedure OnConfigOpenGL; override;

        public
            {**
            * Constructor
            *@param pOwner - component owner
            *}
            constructor Create(pOwner: TComponent); override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Copies the property attributes from another property
            *@param pSource - source property to copy from
            *}
            procedure Assign(pSource: TPersistent); override;

        published
            { Properties }
            property OnDrawSceneFramedModelItem: TQRDrawSceneStaticModelItemEvent read m_fDrawSceneStaticModelItemEvent write m_fDrawSceneStaticModelItemEvent;
    end;

    {**
    * Called when a framed model item should be drawn on the scene
    *@param pSender - event sender
    *@param hDC - device context
    *@param hGLRC - OpenGL render context
    *@param pRenderer - OpenGL renderer
    *@param pShader - OpenGL shader
    *@param pGroup - group at which model belongs
    *@param pModel - model to draw
    *@param textures - textures belonging to model, in the order where they should be combined
    *@param matrix - model matrix
    *@param index - model mesh index
    *@param nextIndex - model mesh index to interpolate with
    *@param interpolationFactor - interpolation factor
    *@param pMesh - mesh to draw, can be nil (depends on selected options)
    *@param pNextMesh - next mesh to interpolate with, can be nil (depends on selected options)
    *@param pAABBTree - aligned-axis bounding box tree matching with mesh, can be nil (depends on selected options)
    *@param pNextAABBTree - aligned-axis bounding box tree matching with next mesh, can be nil (depends on selected options)
    *@return true if framed model item was drawn on the scene, otherwise false
    *}
    TQRDrawSceneFramedModelItemEvent = function (pSender: TObject;
                                              hDC, hGLRC: THandle;
                                               pRenderer: TQRVCLModelRendererGL;
                                                 pShader: TQRVCLModelShaderGL;
                                            const pGroup: TQRModelGroup;
                                            const pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4;
                                        index, nextIndex: NativeInt;
                               const interpolationFactor: Double;
                                  const pMesh, pNextMesh: PQRMesh;
                          const pAABBTree, pNextAABBTree: TQRAABBTree): Boolean of object;

    {**
    * Basic model component supporting framed animation and using the VCL and OpenGL to draw it
    *}
    TQRVCLFramedModelComponentGL = class(TQRVCLModelComponentGL)
        protected
            m_ElapsedTime:                    Double;
            m_NoAnimation:                    Boolean;
            m_fDrawSceneFramedModelItemEvent: TQRDrawSceneFramedModelItemEvent;

            {**
            * Called when OpenGL can be configured
            *}
            procedure OnConfigOpenGL; override;

        public
            {**
            * Constructor
            *@param pOwner - component owner
            *}
            constructor Create(pOwner: TComponent); override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Called when subject send a notification to the observer
            *@param message - notification message
            *}
            procedure OnNotified(message: TQRMessage); override;

            {**
            * Copies the property attributes from another property
            *@param pSource - source property to copy from
            *}
            procedure Assign(pSource: TPersistent); override;

        published
            { Properties }
            property NoAnimation:                Boolean                          read m_NoAnimation                    write m_NoAnimation                    default False;
            property OnDrawSceneFramedModelItem: TQRDrawSceneFramedModelItemEvent read m_fDrawSceneFramedModelItemEvent write m_fDrawSceneFramedModelItemEvent;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLModelComponent
//--------------------------------------------------------------------------------------------------
constructor TQRVCLModelComponentGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // update control style to use
    ControlStyle := ControlStyle + [csPannable, csReplicatable];
    ControlStyle := ControlStyle - [csOpaque, csFramed];

    // initialize local variables
    m_pDefaultImage        := TPngImage.Create;
    m_pColor               := TQRVCLModelComponentColorGL.Create(Self, OnReceivePropNotification);
    m_pAlphaBlending       := TQRVCLModelComponentAlphaBlendingPropertyGL.Create(Self, OnReceivePropNotification);
    m_pRenderer            := TQRVCLModelRendererGL.Create;
    m_pShader              := TQRVCLModelShaderGL.Create;
    m_pRenderSurface       := TQRVCLModelRenderSurfaceGL.Create(Self, m_pRenderer);
    m_pOverlay             := nil;
    m_pAntialiasingOverlay := nil;
    m_hBackgroundBrush     := 0;
    m_AntialiasingMode     := EQR_AM_None;
    m_UseShader            := False;
    m_SupportsGDI          := True;
    m_LogMessageLoop       := False;
    m_Allowed              := False;
    m_fOnConfigureOpenGL   := nil;
    m_fOnLoadTexture       := nil;
    m_fOnCreateSceneMatrix := nil;
    m_fOnInitializeScene   := nil;
    m_fOnBeforeDrawScene   := nil;
    m_fOnAfterDrawScene    := nil;
    m_fOnFinalizeScene     := nil;
    m_fOnDetectCollisions  := nil;

    // configure some default properties
    ParentBackground := False;
    DoubleBuffered   := True;
    Width            := 100;
    Height           := 100;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLModelComponentGL.Destroy;
begin
    // detach from animation timer and stop to receive time notifications (runtime only)
    if (csDesigning in ComponentState) then
        TQRDesignerHook.GetInstance().Detach(Self);

    // clear memory. NOTE overlay structures are deleted while handle is destroyed
    m_pRenderSurface.Free;
    m_pShader.Free;
    m_pRenderer.Free;
    m_pAlphaBlending.Free;
    m_pColor.Free;
    m_pDefaultImage.Free;

    // delete background brush
    if (m_hBackgroundBrush <> 0) then
    begin
        DeleteObject(m_hBackgroundBrush);
        m_hBackgroundBrush := 0;
    end;

    // release draw context, if exists
    ReleaseDrawContext;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.WndProc(var message: TMessage);
var
    hDC: THandle;
    ps:  PAINTSTRUCT;
begin
    {$IFDEF DEBUG}
        // log message loop, if activated
        if (m_LogMessageLoop) then
            TQRLogHelper.LogToCompiler(TQRLogHelper.WinMsgToStr(Self, message));
    {$ENDIF}

    // dispatch message
    case (message.Msg) of
        WM_ERASEBKGND:
        begin
            // as scene background is always filled by OpenGL, ignore message to prevent ugly
            // flickering while scene is drawn. NOTE user is responsible to clear background before
            // drawing a transparent or translucent scene, by handling the OnInitializeScene event
            message.Result := 0;
            Exit;
        end;

        WM_PAINT:
        begin
            // handle is allocated and component isn't currently destroying?
            if (HandleAllocated and (not(csDestroying in ComponentState))) then
            begin
                // although this is not documented inside the MS documentation, sometimes the device
                // context to use may be sent inside the wParam property. If it's the case, use it
                hDC := message.WParam;

                // do use the provided device context?
                if (hDC <> 0) then
                    DrawScene(hDC)
                else
                    try
                        // begin to paint the scene
                        hDC := BeginPaint(WindowHandle, ps);

                        // draw the scene
                        DrawScene(hDC);
                    finally
                        // end scene painting
                        EndPaint(WindowHandle, ps);
                    end;

                // validate entire client rect and exit (it has just been completely redrawn)
                ValidateRect(WindowHandle, nil);
                Exit;
            end;
        end;

        WM_PRINT:
        begin
            if (DrawSceneTo(message)) then
                Exit;
        end;

        WM_PRINTCLIENT:
        begin
            if (DrawSceneTo(message)) then
                Exit;
        end;
    end;

    inherited WndProc(message);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.SetAntialiasingMode(mode: EQRAntialiasingMode);
begin
    // nothing to do?
    if (mode = m_AntialiasingMode) then
        Exit;

    m_AntialiasingMode := mode;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.GetAntialiasingFactor: NativeInt;
begin
    case (m_antialiasingMode) of
        EQR_AM_None:    Result := 1;
        EQR_AM_FSAA_2x: Result := 2;
        EQR_AM_FSAA_4x: Result := 4;
        EQR_AM_FSAA_8x: Result := 8;
    else
        raise Exception.CreateFmt('Unknown antialiasing mode - %d', [Integer(m_AntialiasingMode)]);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.Loaded;
var
    pControlToHook: TWinControl;
    pDesignerHook:  TQRDesignerHook;
begin
    inherited Loaded;

    // initialize some special stuff used only in design time
    if (csDesigning in ComponentState) then
    begin
        // get control parent
        pControlToHook := Parent;

        // iterate through control hierarchy
        while Assigned(pControlToHook) do
        begin
            // found designer control to hook?
            if ((pControlToHook.ClassName = 'TFormContainerForm') and
                (pControlToHook is TWinControl))
            then
            begin
                // get designer hook instance
                pDesignerHook := TQRDesignerHook.GetInstance;

                // found it?
                if (Assigned(pDesignerHook)) then
                begin
                    // hook the editor form designer control
                    pDesignerHook.SetHookedControl(TWinControl(pControlToHook));
                    pDesignerHook.AddFilter(WM_HSCROLL);
                    pDesignerHook.AddFilter(WM_VSCROLL);
                    pDesignerHook.Attach(Self);
                end;

                Break;
            end;

            // go to next parent
            pControlToHook := pControlToHook.Parent;
        end;
    end
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.Resize;
begin
    inherited Resize;

    // resize viewport
    CreateViewport(ClientWidth, ClientHeight);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.CreateParams(var params: TCreateParams);
begin
    inherited CreateParams(params);

    // update Windows class to update automatically the paint every time the control is resized
    params.WindowClass.Style := params.WindowClass.style or CS_HREDRAW or CS_VREDRAW;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.CreateWindowHandle(const params: TCreateParams);
var
    hDC:    THandle;
    factor: NativeInt;
begin
    // release previous draw context, if exists
    ReleaseDrawContext;

    inherited CreateWindowHandle(params);

    // check if handle was successfully allocated
    if (not HandleAllocated) then
        Exit;

    // get the device context for this control
    hDC := GetDC(WindowHandle);

    // found it?
    if (hDC = 0) then
        Exit;

    try
        factor := GetAntialiasingFactor;

        // initialize new render surface instance
        m_Allowed := m_pRenderSurface.Initialize(hDC,
                                                 factor,
                                                 m_pAlphaBlending.Enabled,
                                                 m_SupportsGDI);

        // do use antialiasing?
        if (factor <> 1) then
        begin
            // create and configure local overlay
            m_pAntialiasingOverlay             := Vcl.Graphics.TBitmap.Create;
            m_pAntialiasingOverlay.PixelFormat := pf32bit;
            m_pAntialiasingOverlay.AlphaFormat := afPremultiplied;
        end;

        // do use alpha transparency?
        if (m_pAlphaBlending.Enabled) then
        begin
            // create and configure local overlay
            m_pOverlay             := Vcl.Graphics.TBitmap.Create;
            m_pOverlay.PixelFormat := pf32bit;
            m_pOverlay.AlphaFormat := afPremultiplied;
        end;

        // configure OpenGL
        OnConfigOpenGL;

        // notify user that OpenGL can be configured
        if (m_Allowed and Assigned(m_fOnConfigureOpenGL)) then
            m_fOnConfigureOpenGL(Self,
                                 hDC,
                                 m_pRenderSurface.GLContext,
                                 m_pRenderer,
                                 m_pShader);

        // create background brush
        m_hBackgroundBrush := CreateSolidBrush(ColorToRGB(clBlack));

        // create a viewport for the scene
        CreateViewport(ClientWidth, ClientHeight);
    finally
        ReleaseDC(WindowHandle, hDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.DestroyWindowHandle;
begin
    // delete background brush
    if (m_hBackgroundBrush <> 0) then
    begin
        DeleteObject(m_hBackgroundBrush);
        m_hBackgroundBrush := 0;
    end;

    // release draw context, if exists
    ReleaseDrawContext;

    inherited DestroyWindowHandle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.ReleaseDrawContext;
var
    hDC: THandle;
begin
    // clear overlay
    m_pOverlay.Free;
    m_pOverlay := nil;

    // clear antialiasing overlay
    m_pAntialiasingOverlay.Free;
    m_pAntialiasingOverlay := nil;

    // check if handle was successfully allocated
    if (HandleAllocated) then
    begin
        // get the device context for this control
        hDC := GetDC(WindowHandle);

        // found it?
        if (hDC = 0) then
            Exit;

        try
            // release render surface instance, if exists
            m_pRenderSurface.Release(hDC);
        finally
            ReleaseDC(WindowHandle, hDC);
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.CreateViewport(width, height: NativeUInt);
var
    factor:                  NativeInt;
    position, direction, up: TQRVector3D;
    hDC:                     THandle;
begin
    // cannot create a viewport if there is no client surface to render to it
    if ((ClientWidth = 0) or (ClientHeight = 0)) then
        Exit;

    // no render surface?
    if (not Assigned(m_pRenderSurface)) then
        Exit;

    // check if handle was successfully allocated
    if (not HandleAllocated) then
        Exit;

    // get the device context for this control
    hDC := GetDC(WindowHandle);

    // found it?
    if (hDC = 0) then
        Exit;

    try
        // enable render surface context
        if (not m_pRenderSurface.EnableContext(hDC)) then
            Exit;

        // resize render surface
        m_pRenderSurface.Resize(hDC);

        // resize local overlay, if any
        if (Assigned(m_pOverlay)) then
            m_pOverlay.SetSize(ClientWidth, ClientHeight);

        // OpenGL was initialized correctly?
        if (m_Allowed) then
        begin
            // get antialiasing factor to apply
            factor := GetAntialiasingFactor;

            // create OpenGL viewport to use to draw scene
            m_pRenderer.CreateViewport(ClientWidth * factor, ClientHeight * factor);

            // notify user that scene matrix (i.e. projection and view matrix) are about to be created
            if (Assigned(m_fOnCreateSceneMatrix)) then
                // user defined his own matrix?
                if (m_fOnCreateSceneMatrix(Self, m_ProjectionMatrix, m_ViewMatrix)) then
                    Exit;

            // create projection matrix (will not be modified while execution)
            m_ProjectionMatrix := m_pRenderer.GetOrtho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);

            position  := TQRVector3D.Create(0.0, 0.0, 0.0);
            direction := TQRVector3D.Create(0.0, 0.0, 1.0);
            up        := TQRVector3D.Create(0.0, 1.0, 0.0);

            // create view matrix (will not be modified while execution)
            m_ViewMatrix := m_pRenderer.LookAtLH(position, direction, up);

            // do use shader?
            if (not m_UseShader) then
            begin
                // load projection matrix and initialize it
                glMatrixMode(GL_PROJECTION);
                glLoadIdentity();

                // apply projection matrix
                glLoadMatrix(PGLfloat(m_ProjectionMatrix.GetPtr()));

                // load model view matrix and initialize it
                glMatrixMode(GL_MODELVIEW);
                glLoadIdentity();

                // apply model view matrix
                glLoadMatrix(PGLfloat(m_ViewMatrix.GetPtr()));
            end;
        end;
    finally
        ReleaseDC(WindowHandle, hDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.BuildShader(pVertexPrg, pFragmentPrg: TStream): Boolean;
begin
    // OpenGL was not initialized correctly?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    // load and compile shader
    m_pShader.CreateProgram();
    m_pShader.AttachFile(pVertexPrg,   EQR_ST_Vertex);
    m_pShader.AttachFile(pFragmentPrg, EQR_ST_Fragment);

    // try to link shader
    Result := m_pShader.Link(False);
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.DrawDefaultImage(hDC: THandle): Boolean;
var
    hPackageInstance: NativeUInt;
    pStream:          TResourceStream;
    pBitmap:          Vcl.Graphics.TBitmap;
begin
    // is component currently deleting?
    if (csDestroying in ComponentState) then
    begin
        Result := False;
        Exit;
    end;

    // no device context to draw to?
    if (hDC = 0) then
    begin
        Result := False;
        Exit;
    end;

    pStream := nil;

    // is default image still not loaded?
    if ((m_pDefaultImage.Width = 0) or (m_pDefaultImage.Height = 0)) then
        try
            // get module instance at which this control belongs
            hPackageInstance := FindClassHInstance(TQRVCLModelComponentGL);

            // found module and package contains the default image?
            if ((hPackageInstance = 0) or
                (FindResource(hPackageInstance, PChar('RC_BROKEN_MODEL_IMAGE'), RT_RCDATA) = 0))
            then
            begin
                Result := False;
                Exit;
            end;

            // load normals table from stream
            pStream := TResourceStream.Create(hPackageInstance,
                                              PChar('RC_BROKEN_MODEL_IMAGE'),
                                              RT_RCDATA);

            // load default image from stream
            m_pDefaultImage.LoadFromStream(pStream);
        finally
            // delete resource stream, if needed
            pStream.Free
        end;

    pBitmap := nil;

    try
        // create a new bitmap image
        pBitmap             := Vcl.Graphics.TBitmap.Create;
        pBitmap.PixelFormat := pf32bit;
        pBitmap.AlphaFormat := afDefined;
        pBitmap.SetSize(m_pDefaultImage.Width, m_pDefaultImage.Height);

        // clear background to make it transparent
        pBitmap.Canvas.Brush.Style := bsClear;
        pBitmap.Canvas.Brush.Color := Vcl.Graphics.clNone;
        pBitmap.Canvas.FillRect(TRect.Create(0, 0, m_pDefaultImage.Width, m_pDefaultImage.Height));

        // copy default image into bitmap
        pBitmap.Canvas.Draw(0, 0, m_pDefaultImage);

        // draw default image
        if ((ClientWidth >= 100) and (ClientHeight >= 100)) then
            BitBlt(hDC,
                   (ClientWidth  shr 1) - (m_pDefaultImage.Width  shr 1),
                   (ClientHeight shr 1) - (m_pDefaultImage.Height shr 1),
                   m_pDefaultImage.Width,
                   m_pDefaultImage.Height,
                   pBitmap.Canvas.Handle,
                   0,
                   0,
                   SRCCOPY)
        else
            StretchBlt(hDC,
                       0,
                       0,
                       ClientWidth,
                       ClientHeight,
                       pBitmap.Canvas.Handle,
                       0,
                       0,
                       m_pDefaultImage.Width,
                       m_pDefaultImage.Height,
                       SRCCOPY);
    finally
        pBitmap.Free;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.DrawScene(hDC: THandle);
var
    bf:                                            _BLENDFUNCTION;
    factor, antialiasingWidth, antialiasingHeight: NativeInt;
begin
    // check if handle was already created, create it if not
    HandleNeeded;

    // no received device context to draw to?
    if (hDC = 0) then
        Exit;

    try
        // not allowed to draw the scene? (i.e. OpenGL was not initialized correctly)
        if (not m_Allowed) then
        begin
            // FIXME add OnDrawDefaultImage
            DrawDefaultImage(hDC);
            Exit;
        end;

        // for the design time, paint the background in black to avoid visual artifacts if alpha
        // blending is enabled
        if ((m_pAlphaBlending.Enabled)      and
            (csDesigning in ComponentState) and
            (m_hBackgroundBrush <> 0))
        then
            FillRect(hDC, TRect.Create(0, 0, ClientWidth, ClientHeight), m_hBackgroundBrush);

        // notify user that scene is initialized and ready to be drawn
        if (not m_pAlphaBlending.Enabled and m_SupportsGDI and Assigned(m_fOnInitializeScene)) then
            m_fOnInitializeScene(Self, hDC);

        // render surface was correctly initialized?
        if (not Assigned(m_pRenderSurface)) then
            Exit;

        // resize antialiasing overlay, if any
        if (Assigned(m_pAntialiasingOverlay)) then
        begin
            factor             := GetAntialiasingFactor;
            antialiasingWidth  := ClientWidth  * factor;
            antialiasingHeight := ClientHeight * factor;

            if ((m_pAntialiasingOverlay.Width  <> antialiasingWidth) or
                (m_pAntialiasingOverlay.Height <> antialiasingHeight))
            then
                m_pAntialiasingOverlay.SetSize(antialiasingWidth, antialiasingHeight);
        end
        else
        begin
            factor := 1;
        end;

        // resize render surface
        m_pRenderSurface.Resize(hDC);

        // resize local overlay, if any
        if (Assigned(m_pOverlay) and
          ((m_pOverlay.Width <> ClientWidth * factor) or (m_pOverlay.Height <> ClientHeight * factor)))
        then
            m_pOverlay.SetSize(ClientWidth * factor, ClientHeight * factor);

        // begin to draw scene on overlay surface
        if (not m_pRenderSurface.BeginScene(hDC)) then
            Exit;

        // clear the scene
        glClearColor(m_pColor.RedF, m_pColor.GreenF, m_pColor.BlueF, m_pColor.AlphaF);
        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

        // notify user that scene is about to be drawn
        if (Assigned(m_fOnBeforeDrawScene)) then
            m_fOnBeforeDrawScene(Self,
                                 hDC,
                                 m_pRenderSurface.GLContext,
                                 m_pRenderer,
                                 m_pShader);

        // draw the scene
        OnDrawSceneContent(hDC);

        // notify user that scene is drawn
        if (Assigned(m_fOnAfterDrawScene)) then
            m_fOnAfterDrawScene(Self,
                                hDC,
                                m_pRenderSurface.GLContext,
                                m_pRenderer,
                                m_pShader);

        // end scene on render surface
        m_pRenderSurface.EndScene(hDC);

        // is antialiasing or alpha blending enabled?
        if (factor <> 1) then
        begin
            // get drawn scene as bitmap
            m_pRenderSurface.GetBitmap(hDC, m_pAntialiasingOverlay);

            // notify user that scene is initialized and ready to be drawn. NOTE the scene is
            // initialized after OpenGL painted it, because an overlay was used in this case, and
            // initializing scene just before copying overlay reduce flickering
            if (Assigned(m_fOnInitializeScene)) then
                m_fOnInitializeScene(Self, hDC);

            // apply antialiasing
            TQRGDIHelper.ApplyAntialiasing(m_pAntialiasingOverlay.Canvas.Handle,
                                           hDC,
                                           0,
                                           0,
                                           ClientWidth,
                                           ClientHeight,
                                           factor);
        end
        else
        if (m_pAlphaBlending.Enabled) then
        begin
            // get drawn scene as bitmap
            m_pRenderSurface.GetBitmap(hDC, m_pOverlay);

            // notify user that scene is initialized and ready to be drawn. NOTE the scene is
            // initialized after OpenGL painted it, because an overlay was used in this case, and
            // initializing scene just before copying overlay reduce flickering
            if (Assigned(m_fOnInitializeScene)) then
                m_fOnInitializeScene(Self, hDC);

            // configure alpha blending operation
            bf.BlendOp             := AC_SRC_OVER;
            bf.BlendFlags          := 0;
            bf.SourceConstantAlpha := m_pAlphaBlending.GlobalLevel;
            bf.AlphaFormat         := AC_SRC_ALPHA;

            // copy image to final device context applying alpha blending
            AlphaBlend(hDC,
                       0,
                       0,
                       ClientWidth,
                       ClientHeight,
                       m_pOverlay.Canvas.Handle,
                       0,
                       0,
                       m_pOverlay.Width,
                       m_pOverlay.Height,
                       bf);
        end;
    finally
        // notify user that scene is completely drawn
        if (m_SupportsGDI and Assigned(m_fOnFinalizeScene)) then
            m_fOnFinalizeScene(Self, hDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.DrawSceneTo(var message: TMessage): Boolean;
var
    hDC:              THandle;
    hBackgroundBrush: HBRUSH;
begin
    // is component currently destroying?
    if (csDestroying in ComponentState) then
    begin
        Result := False;
        Exit;
    end;

    hDC := THandle(message.WParam);

    if (hDC = 0) then
    begin
        Result := False;
        Exit;
    end;

    // do check if control is visible before draw scene to device context?
    if ((message.LParam and PRF_CHECKVISIBLE) <> 0) then
        if (not Visible) then
        begin
            Result := False;
            Exit;
        end;

    // do erase background before draw the scene to the device context?
    if ((message.LParam and PRF_ERASEBKGND) <> 0) then
    begin
        hBackgroundBrush := 0;

        try
            // create a brush to paint the background
            hBackgroundBrush := CreateSolidBrush(ColorToRGB(Color.VCLColor));

            // succeeded?
            if (hBackgroundBrush <> 0) then
                // paint the background
                FillRect(hDC,
                         TRect.Create(0, 0, ClientWidth, ClientHeight),
                         hBackgroundBrush);
        finally
            // clear memory
            if (hBackgroundBrush <> 0) then
                DeleteObject(hBackgroundBrush);
        end;
    end;

    // draw the scene
    DrawSceneTo(hDC, 0, 0);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.DrawSceneTo(hDC: THandle; x, y: Integer);
var
    bf:         _BLENDFUNCTION;
    pOverlay:   Vcl.Graphics.TBitmap;
    hControlDC: THandle;
    factor:     NativeInt;
begin
    // check if handle was successfully allocated
    if (not HandleAllocated) then
        Exit;

    // destination device context should at least support the bit blitting
    if ((GetDeviceCaps(hDC, RASTERCAPS) and RC_BITBLT) = 0) then
        raise Exception.Create('Destination device context does not support the bit blitting');

    pOverlay := nil;

    // get the device context for this control
    hControlDC := GetDC(WindowHandle);

    // found it?
    if (hControlDC = 0) then
        Exit;

    try
        // draw the scene in a normal way
        DrawScene(hControlDC);

        // get antialiasing factor to apply
        factor := GetAntialiasingFactor;

        // create a new temporary overlay to receive the previously drawn scene
        pOverlay             := Vcl.Graphics.TBitmap.Create;
        pOverlay.PixelFormat := pf32bit;
        pOverlay.AlphaFormat := afPremultiplied;
        pOverlay.SetSize(ClientWidth * factor, ClientHeight * factor);

        // get drawn scene as bitmap
        m_pRenderSurface.GetBitmap(hControlDC, pOverlay);

        // is antialiasing enabled?
        if (factor <> 1) then
        begin
            // apply antialiasing
            TQRGDIHelper.ApplyAntialiasing(pOverlay.Canvas.Handle,
                                           hDC,
                                           0,
                                           0,
                                           ClientWidth,
                                           ClientHeight,
                                           factor);
        end
        else
        // is alpha blending supported by the destination device context?
        if (GetDeviceCaps(hDC, SHADEBLENDCAPS) = SB_NONE) then
        begin
            // no, use a simple bitmap blitter
            BitBlt(hDC,
                   x,
                   y,
                   ClientWidth,
                   ClientHeight,
                   pOverlay.Canvas.Handle,
                   0,
                   0,
                   SRCCOPY);
        end
        else
        begin
            // configure alpha blending operation
            bf.BlendOp             := AC_SRC_OVER;
            bf.BlendFlags          := 0;
            bf.SourceConstantAlpha := m_pAlphaBlending.GlobalLevel;
            bf.AlphaFormat         := AC_SRC_ALPHA;

            // copy image to final device context applying alpha blending
            AlphaBlend(hDC,
                       x,
                       y,
                       ClientWidth,
                       ClientHeight,
                       pOverlay.Canvas.Handle,
                       0,
                       0,
                       pOverlay.Width,
                       pOverlay.Height,
                       bf);
        end;
    finally
        ReleaseDC(WindowHandle, hControlDC);
        pOverlay.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.OnReceivePropNotification(pSender: TObject;
                                                          message: EQRPropMessages): Boolean;
begin
    // dispatch message
    case (message) of
        EQR_PM_RecreateWnd: RecreateWnd;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.OnNotified(message: TQRMessage);
var
    info: TQRDesignerHookMsgInfo;
begin
    // received a message from designer hook controller?
    case (EQRDesignerHookMessages(message.m_Type)) of
        EQR_DH_Message:
        begin
            // get designer Windows message info
            info := TQRDesignerHookMsgInfo(message.m_pInfo^);

            // dispatch message
            case (info.m_Message) of
                WM_HSCROLL: Invalidate;
                WM_VSCROLL: Invalidate;
            end;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.PaintTo(dc: HDC; x, y: Integer);
begin
    DrawSceneTo(dc, x, y);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLModelComponentGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLModelComponentGL)) then
    begin
        // reset values to default
        m_LogMessageLoop       := False;
        m_fOnConfigureOpenGL   := nil;
        m_fOnLoadTexture       := nil;
        m_fOnCreateSceneMatrix := nil;
        m_fOnInitializeScene   := nil;
        m_fOnBeforeDrawScene   := nil;
        m_fOnAfterDrawScene    := nil;
        m_fOnFinalizeScene     := nil;
        m_fOnDetectCollisions  := nil;

        m_pColor.Assign(nil);
        m_pAlphaBlending.Assign(nil);
        Exit;
    end;

    // copy content from source
    pSrc                   := pSource as TQRVCLModelComponentGL;
    m_LogMessageLoop       := pSrc.m_LogMessageLoop;
    m_fOnConfigureOpenGL   := pSrc.m_fOnConfigureOpenGL;
    m_fOnLoadTexture       := pSrc.m_fOnLoadTexture;
    m_fOnCreateSceneMatrix := pSrc.m_fOnCreateSceneMatrix;
    m_fOnInitializeScene   := pSrc.m_fOnInitializeScene;
    m_fOnBeforeDrawScene   := pSrc.m_fOnBeforeDrawScene;
    m_fOnAfterDrawScene    := pSrc.m_fOnAfterDrawScene;
    m_fOnFinalizeScene     := pSrc.m_fOnFinalizeScene;
    m_fOnDetectCollisions  := pSrc.m_fOnDetectCollisions;

    m_pColor.Assign(pSrc.m_pColor);
    m_pAlphaBlending.Assign(pSrc.m_pAlphaBlending);
end;
//--------------------------------------------------------------------------------------------------
// TQRVCLStaticModelComponentGL
//--------------------------------------------------------------------------------------------------
constructor TQRVCLStaticModelComponentGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize variables
    m_fDrawSceneStaticModelItemEvent := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLStaticModelComponentGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLStaticModelComponentGL.OnConfigOpenGL;
var
    hDC: THandle;
begin
    // OpenGL was not initialized correctly?
    if (not m_Allowed) then
        Exit;

    // enable and configure depth testing
    glEnable(GL_DEPTH_TEST);

    // enable and configure culling
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    // enable and configure texture rendering
    glEnable(GL_TEXTURE_2D);

    // notify that optional OpenGL configuration can be enabled
    if (Assigned(m_fOnConfigureOpenGL)) then
    begin
        // a Windows handle is required to create a device context
        HandleNeeded;

        // failed to create Windows handle?
        if (not HandleAllocated) then
            Exit;

        // get the device context for this control
        hDC := GetDC(WindowHandle);

        try
            m_fOnConfigureOpenGL(Self,
                                 hDC,
                                 m_pRenderSurface.GLContext,
                                 m_pRenderer,
                                 m_pShader);
        finally
            // free the device context
            ReleaseDC(WindowHandle, hDC)
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLStaticModelComponentGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLStaticModelComponentGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLStaticModelComponentGL)) then
    begin
        // reset values to default
        m_fDrawSceneStaticModelItemEvent := nil;
        Exit;
    end;

    // copy content from source
    pSrc                             := pSource as TQRVCLStaticModelComponentGL;
    m_fDrawSceneStaticModelItemEvent := pSrc.m_fDrawSceneStaticModelItemEvent;
end;
//--------------------------------------------------------------------------------------------------
// TQRVCLAnimatedModelComponentGL
//--------------------------------------------------------------------------------------------------
constructor TQRVCLFramedModelComponentGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize values
    m_ElapsedTime                    := 0.0;
    m_NoAnimation                    := False;
    m_fDrawSceneFramedModelItemEvent := nil;

    // attach to animation timer to receive time notifications (runtime only)
    if (not(csDesigning in ComponentState)) then
        TQRVCLAnimationTimer.GetInstance().Attach(Self);
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLFramedModelComponentGL.Destroy;
begin
    // detach from animation timer and stop to receive time notifications (runtime only)
    if (not(csDesigning in ComponentState)) then
        TQRVCLAnimationTimer.GetInstance().Detach(Self);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLFramedModelComponentGL.OnConfigOpenGL;
var
    hDC: THandle;
begin
    // OpenGL was not initialized correctly?
    if (not m_Allowed) then
        Exit;

    // enable and configure depth testing
    glEnable(GL_DEPTH_TEST);

    // enable and configure culling
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);

    // enable and configure texture rendering
    glEnable(GL_TEXTURE_2D);

    // notify that optional OpenGL configuration can be enabled
    if (Assigned(m_fOnConfigureOpenGL)) then
    begin
        // a Windows handle is required to create a device context
        HandleNeeded;

        // failed to create Windows handle?
        if (not HandleAllocated) then
            Exit;

        // get the device context for this control
        hDC := GetDC(WindowHandle);

        try
            m_fOnConfigureOpenGL(Self,
                                 hDC,
                                 m_pRenderSurface.GLContext,
                                 m_pRenderer,
                                 m_pShader);
        finally
            // free the device context
            ReleaseDC(WindowHandle, hDC)
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLFramedModelComponentGL.OnNotified(message: TQRMessage);
var
    info: TQRVCLAnimationTimerMsgInfo;
    hDC:  THandle;
begin
    inherited OnNotified(message);

    // received a message from animation timer?
    case (EQRVCLAnimationTimerMessages(message.m_Type)) of
        EQR_AM_Animate:
        begin
            // don't animate?
            if (m_NoAnimation) then
                Exit;

            // check if handle was successfully allocated
            if (not HandleAllocated) then
                Exit;

            // do nothing if the control isn't visible
            if ((not Visible) or (not IsWindowVisible(WindowHandle))) then
                Exit;

            // get elapsed time from message info sructure
            info          := TQRVCLAnimationTimerMsgInfo(message.m_pInfo^);
            m_ElapsedTime := info.m_ElapsedTime;

            // get the device context for this control
            hDC := GetDC(WindowHandle);

            // found it?
            if (hDC = 0) then
                Exit;

            try
                DrawScene(hDC);
            finally
                ReleaseDC(WindowHandle, hDC);
            end;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLFramedModelComponentGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLFramedModelComponentGL;
begin
    inherited Assign(pSource);

    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLFramedModelComponentGL)) then
    begin
        // reset values to default
        m_fDrawSceneFramedModelItemEvent := nil;
        Exit;
    end;

    // copy content from source
    pSrc                             := pSource as TQRVCLFramedModelComponentGL;
    m_fDrawSceneFramedModelItemEvent := pSrc.m_fDrawSceneFramedModelItemEvent;
end;
//--------------------------------------------------------------------------------------------------

end.
