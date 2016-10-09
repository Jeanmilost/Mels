{**************************************************************************************************
 * ==> UTQRVCLShapeComponentGL -------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides a set of shapes that can be drawn using the VCL and OpenGL. *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLShapeComponentGL;

interface
    // do not include XE7.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE XE7.OpenGLext *)

uses System.Classes,
     System.SysUtils,
     UTQRHelpers,
     UTQRVCLHelpers,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRModel,
     UTQRModelGroup,
     UTQRShapes,
     UTQRShapeGroup,
     UTQRVCLModelComponentGL,
     UTQRVCLModelComponentPropertiesGL,
     Vcl.Graphics,
     Vcl.Controls,
     Winapi.Windows,
     // unfortunately the required OpenGL headers does not exist or are incomplete in XE4 and
     // earlier, so the DelphiGL component (provided with installation) should be used instead
     XE7.OpenGL,
     XE7.OpenGLext;

type
    {**
    * Shapes component
    *}
    TQRVCLShapeGL = class(TQRVCLStaticModelComponentGL)
        protected
            m_pShape:          TQRShapeGroup;
            m_pModel:          TQRVCLModelComponentPropertyGL;
            m_pTexture:        TPicture;
            m_VertexName:      TFileName;
            m_FragmentName:    TFileName;
            m_pVertexShader:   TMemoryStream;
            m_pFragmentShader: TMemoryStream;
            m_ModelOptions:    TQRModelOptions;
            m_hSceneDC:        THandle;

            {**
            * Sets texture
            *@param pPicture - texture picture
            *}
            procedure SetTexture(pPicture: TPicture); virtual;

            {**
            * Sets model options
            *@param options - options
            *}
            procedure SetModelOptions(options: TQRModelOptions); virtual;

            {**
            * Sets model vertex shader file name
            *@param fileName - file name
            *}
            procedure SetVertexName(fileName: TFileName); virtual;

            {**
            * Sets model fragment shader file name
            *@param fileName - file name
            *}
            procedure SetFragmentName(fileName: TFileName); virtual;

            {**
            * Declares properties that will deal with DFM files
            *@param pFiler - DFM file manager
            *}
            procedure DefineProperties(pFiler: TFiler); override;

            {**
            * Reads vertex shader content from DFM file
            *@param pStream - stream containing DFM data
            *}
            procedure ReadVertexShader(pStream: TStream); virtual;

            {**
            * Writes vertex shader content to DFM file
            *@param pStream - DFM stream in which vertex shader should be written
            *}
            procedure WriteVertexShader(pStream: TStream); virtual;

            {**
            * Reads fragment shader content from DFM file
            *@param pStream - stream containing DFM data
            *}
            procedure ReadFragmentShader(pStream: TStream); virtual;

            {**
            * Writes fragment shader content to DFM file
            *@param pStream - DFM stream in which fragment shader should be written
            *}
            procedure WriteFragmentShader(pStream: TStream); virtual;

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
            * Loads the model
            *@return true on success, otherwise false
            *}
            function LoadModel: Boolean; virtual; abstract;

            {**
            * Called after model was completely loaded
            *@param pGroup - group that finished to load the model
            *}
            procedure OnAfterLoadModelEvent(const pGroup: TQRModelGroup); virtual;

            {**
            * Called when texture changed
            *@param pSender - event sender
            *}
            procedure OnTextureChanged(pSender: TObject); virtual;

            {**
            * Called when mesh texture should be loaded
            *@param pModel - model for which texture should be loaded
            *@param pBitmap - whenever possible, the bitmap containing the texture, nil if not available
            *@param pTexture - texture info
            *@param[out] loadNext - if true, event will be called again with a new item to load next texture
            *@return true on success, otherwise false
            *}
            function OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                       const pModel: TQRModel;
                                            pBitmap: Vcl.Graphics.TBitmap;
                                           pTexture: TQRTexture;
                                       out loadNext: Boolean): Boolean; virtual;

            {**
            * Called when the scene content should be drawn
            *@param hDC - internal control device context that OpenGL should use to draw the scene
            *}
            procedure OnDrawSceneContent(hDC: THandle); override;

            {**
            * Called when framed model item should be drawn
            *@param pGroup - group at which model belongs
            *@param pModel - model to draw
            *@param textures - textures belonging to model, in the order where they should be combined
            *@param matrix - model matrix
            *}
            procedure OnCustomDrawModelItem(const pGroup: TQRModelGroup;
                                                  pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4); virtual;

            {**
            * Called when framed model item should be drawn
            *@param pGroup - group at which model belongs
            *@param pModel - model to draw
            *@param textures - textures belonging to model, in the order where they should be combined
            *@param matrix - model matrix
            *@param pMesh - mesh to draw, can be NULL
            *@param pAABBTree - aligned-axis bounding box tree matching with mesh, can be NULL
            *}
            procedure OnDrawModelItem(const pGroup: TQRModelGroup;
                                      const pModel: TQRModel;
                                    const textures: TQRTextures;
                                      const matrix: TQRMatrix4x4;
                                       const pMesh: PQRMesh;
                                   const pAABBTree: TQRAABBTree); virtual;

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
            property Model:        TQRVCLModelComponentPropertyGL read m_pModel       write m_pModel;
            property ModelOptions: TQRModelOptions                read m_ModelOptions write SetModelOptions default [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];
            property VertexName:   TFileName                      read m_VertexName   write SetVertexName;
            property FragmentName: TFileName                      read m_FragmentName write SetFragmentName;
            property Texture:      TPicture                       read m_pTexture     write SetTexture;
    end;

    {**
    * Surface component
    *}
    TQRVCLSurfaceGL = class(TQRVCLShapeGL)
        protected
            m_SurfaceWidth:  Single;
            m_SurfaceHeight: Single;

            {**
            * Sets surface width
            *@param width - surface width
            *}
            procedure SetSurfaceWidth(width: Single); virtual;

            {**
            * Sets surface height
            *@param height - surface height
            *}
            procedure SetSurfaceHeight(height: Single); virtual;

            {**
            * Loads the model
            *@return true on success, otherwise false
            *}
            function LoadModel: Boolean; override;

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
            property SurfaceWidth:  Single read m_SurfaceWidth  write SetSurfaceWidth;
            property SurfaceHeight: Single read m_SurfaceHeight write SetSurfaceHeight;
    end;

    {**
    * Box component
    *}
    TQRVCLBoxGL = class(TQRVCLShapeGL)
        protected
            m_BoxWidth:            Single;
            m_BoxHeight:           Single;
            m_BoxDepth:            Single;
            m_RepeatTexOnEachFace: Boolean;

            {**
            * Sets box width
            *@param width - box width
            *}
            procedure SetBoxWidth(width: Single); virtual;

            {**
            * Sets surface height
            *@param height - surface height
            *}
            procedure SetBoxHeight(height: Single); virtual;

            {**
            * Sets box depth
            *@param depth - box depth
            *}
            procedure SetBoxDepth(depth: Single); virtual;

            {**
            * Loads the model
            *@return true on success, otherwise false
            *}
            function LoadModel: Boolean; override;

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
            property BoxWidth:  Single read m_BoxWidth  write SetBoxWidth;
            property BoxHeight: Single read m_BoxHeight write SetBoxHeight;
            property BoxDepth:  Single read m_BoxDepth  write SetBoxDepth;
    end;

    {**
    * Sphere component
    *}
    TQRVCLSphereGL = class(TQRVCLShapeGL)
        protected
            m_Slices: NativeUInt;
            m_Stacks: NativeUInt;
            m_Radius: Single;

            {**
            * Sets slices
            *@param slices - sphere slices
            *}
            procedure SetSlices(slices: NativeUInt); virtual;

            {**
            * Sets stacks
            *@param stacks - sphere stacks
            *}
            procedure SetStacks(stacks: NativeUInt); virtual;

            {**
            * Sets sphere radius
            *@param radius - sphere radius
            *}
            procedure SetRadius(radius: Single); virtual;

            {**
            * Loads the model
            *@return true on success, otherwise false
            *}
            function LoadModel: Boolean; override;

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
            property Slices: NativeUInt read m_Slices write SetSlices default 20;
            property Stacks: NativeUInt read m_Stacks write SetStacks default 20;
            property Radius: Single     read m_Radius write SetRadius;
    end;

    {**
    * Cone component
    *}
    TQRVCLConeGL = class(TQRVCLShapeGL)
        protected
            m_Faces:         NativeUInt;
            m_ConeHeight:    Single;
            m_TopRadiusX:    Single;
            m_TopRadiusY:    Single;
            m_BottomRadiusX: Single;
            m_BottomRadiusY: Single;
            m_Closing:       EQR_Cone_Closing;

            {**
            * Sets cone faces count
            *@param faces - cone faces
            *}
            procedure SetFaces(faces: NativeUInt); virtual;

            {**
            * Sets cone height
            *@param height - sphere height
            *}
            procedure SetConeHeight(height: Single); virtual;

            {**
            * Sets cone top radius for the x axis
            *@param topRadiusX - top radius for the x axis
            *}
            procedure SetTopRadiusX(topRadiusX: Single); virtual;

            {**
            * Sets cone top radius for the y axis
            *@param topRadiusY - top radius for the y axis
            *}
            procedure SetTopRadiusY(topRadiusY: Single); virtual;

            {**
            * Sets cone bottom radius for the x axis
            *@param bottomRadiusX - bottom radius for the x axis
            *}
            procedure SetBottomRadiusX(bottomRadiusX: Single); virtual;

            {**
            * Sets cone bottom radius for the y axis
            *@param bottomRadiusX - bottom radius for the y axis
            *}
            procedure SetBottomRadiusY(bottomRadiusY: Single); virtual;

            {**
            * Sets cone closing method
            *@param closing - cone cloding method
            *}
            procedure SetClosing(closing: EQR_Cone_Closing); virtual;

            {**
            * Loads the model
            *@return true on success, otherwise false
            *}
            function LoadModel: Boolean; override;

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
            property Faces:         NativeUInt       read m_Faces         write SetFaces         default 20;
            property ConeHeight:    Single           read m_ConeHeight    write SetConeHeight;
            property TopRadiusX:    Single           read m_TopRadiusX    write SetTopRadiusX;
            property TopRadiusY:    Single           read m_TopRadiusY    write SetTopRadiusY;
            property BottomRadiusX: Single           read m_BottomRadiusX write SetBottomRadiusX;
            property BottomRadiusY: Single           read m_BottomRadiusY write SetBottomRadiusY;
            property Closing:       EQR_Cone_Closing read m_Closing       write SetClosing       default EQR_CC_Both;
    end;

    {**
    * Torus component
    *}
    TQRVCLTorusGL = class(TQRVCLShapeGL)
        protected
            m_Slices:         NativeUInt;
            m_FacesPerSlices: NativeUInt;
            m_OuterRadiusX:   Single;
            m_OuterRadiusY:   Single;
            m_InnerRadiusX:   Single;
            m_InnerRadiusY:   Single;

            {**
            * Sets torus slices count
            *@param slices - torus slices
            *}
            procedure SetSlices(slices: NativeUInt); virtual;

            {**
            * Sets torus faces per slices count
            *@param facesPerSlices - torus faces per slices
            *}
            procedure SetFacesPerSlices(facesPerSlices: NativeUInt); virtual;

            {**
            * Sets torus inner radius for the x axis
            *@param innerRadiusX - inner radius for the x axis
            *}
            procedure SetInnerRadiusX(innerRadiusX: Single); virtual;

            {**
            * Sets torus inner radius for the y axis
            *@param innerRadiusY - inner radius for the y axis
            *}
            procedure SetInnerRadiusY(innerRadiusY: Single); virtual;

            {**
            * Sets torus outer radius for the x axis
            *@param outerRadiusX - outer radius for the x axis
            *}
            procedure SetOuterRadiusX(outerRadiusX: Single); virtual;

            {**
            * Sets torus outer radius for the y axis
            *@param outerRadiusX - outer radius for the y axis
            *}
            procedure SetOuterRadiusY(outerRadiusY: Single); virtual;

            {**
            * Loads the model
            *@return true on success, otherwise false
            *}
            function LoadModel: Boolean; override;

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
            property Slices:         NativeUInt read m_Slices         write SetSlices         default 20;
            property FacesPerSlices: NativeUInt read m_FacesPerSlices write SetFacesPerSlices default 20;
            property InnerRadiusX:   Single     read m_InnerRadiusX   write SetInnerRadiusX;
            property InnerRadiusY:   Single     read m_InnerRadiusY   write SetInnerRadiusY;
            property OuterRadiusX:   Single     read m_OuterRadiusX   write SetOuterRadiusX;
            property OuterRadiusY:   Single     read m_OuterRadiusY   write SetOuterRadiusY;
    end;

    {**
    * Parabola component
    *}
    TQRVCLParabolaGL = class(TQRVCLShapeGL)
        protected
            m_Slices:         NativeUInt;
            m_FacesPerSlices: NativeUInt;
            m_ParabolaHeight: Single;
            m_Radius:         Single;

            {**
            * Sets parabola slices count
            *@param slices - parabola slices
            *}
            procedure SetSlices(slices: NativeUInt); virtual;

            {**
            * Sets parabola faces per slices count
            *@param facesPerSlices - parabola faces per slices
            *}
            procedure SetFacesPerSlices(facesPerSlices: NativeUInt); virtual;

            {**
            * Sets parabola height
            *@param height - parabola height
            *}
            procedure SetParabolaHeight(height: Single); virtual;

            {**
            * Sets parabola radius
            *@param radius - parabola radius
            *}
            procedure SetRadius(radius: Single); virtual;

            {**
            * Loads the model
            *@return true on success, otherwise false
            *}
            function LoadModel: Boolean; override;

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
            property Slices:         NativeUInt read m_Slices         write SetSlices;
            property FacesPerSlices: NativeUInt read m_FacesPerSlices write SetFacesPerSlices;
            property ParabolaHeight: Single     read m_ParabolaHeight write SetParabolaHeight;
            property Radius:         Single     read m_Radius         write SetRadius;
    end;

implementation
//------------------------------------------------------------------------------
// TQRVCLShapeGL
//------------------------------------------------------------------------------
constructor TQRVCLShapeGL.Create(pOwner: TComponent);
begin
    // initialize values
    m_pShape          := nil;
    m_pModel          := TQRVCLModelComponentPropertyGL.Create(Self, OnReceivePropNotification);
    m_pTexture        := TPicture.Create;
    m_pVertexShader   := TMemoryStream.Create;
    m_pFragmentShader := TMemoryStream.Create;
    m_ModelOptions    := [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];
    m_hSceneDC        := 0;

    // configure internal components
    m_pTexture.OnChange := OnTextureChanged;

    inherited Create(pOwner);
end;
//------------------------------------------------------------------------------
destructor TQRVCLShapeGL.Destroy;
begin
    // clear memory
    m_pFragmentShader.Free;
    m_pVertexShader.Free;
    m_pTexture.Free;
    m_pModel.Free;
    m_pShape.Free;

    // set explicitly the shape model to nil after deleted it, because in some situations it can be
    // accessed later in destruction, e.g. when handle is destroyed after a control was deleted from
    // designer
    m_pShape := nil;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.SetTexture(pPicture: TPicture);
begin
    m_pTexture.Assign(pPicture);
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.SetModelOptions(options: TQRModelOptions);
begin
    // no changes?
    if (m_ModelOptions = options) then
        Exit;

    m_ModelOptions := options;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.SetVertexName(fileName: TFileName);
var
    fileStream: TFileStream;
begin
    // no changes?
    if (fileName = m_VertexName) then
        Exit;

    // if component is currently loading, just update the property name, the model will be loaded
    // during the serialization
    if (csLoading in ComponentState) then
    begin
        m_VertexName := fileName;
        Exit;
    end;

    // previous vertex shader was loaded?
    if (m_pVertexShader.Size > 0) then
        // clear it
        m_pVertexShader.Clear;

    // file name isn't empty, file exists and is a vertex shader file?
    if ((Length(fileName) = 0)     or
        (not FileExists(fileName)) or
        (LowerCase(ExtractFileExt(fileName)) <> '.glsl'))
    then
    begin
        m_VertexName := '';
        RecreateWnd;
        Exit;
    end;

    // update name and open file stream
    m_VertexName := fileName;
    fileStream   := TFileStream.Create(m_VertexName, fmOpenRead);

    try
        // copy vertex shader file content to memory
        m_pVertexShader.CopyFrom(fileStream, fileStream.Size);
    finally
        fileStream.Free;
    end;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.SetFragmentName(fileName: TFileName);
var
    fileStream: TFileStream;
begin
    // no changes?
    if (fileName = m_FragmentName) then
        Exit;

    // if component is currently loading, just update the property name, the model will be loaded
    // during the serialization
    if (csLoading in ComponentState) then
    begin
        m_FragmentName := fileName;
        Exit;
    end;

    // previous fragment shader was loaded?
    if (m_pFragmentShader.Size > 0) then
        // clear it
        m_pFragmentShader.Clear;

    // file name isn't empty, file exists and is a fragment shader file?
    if ((Length(fileName) = 0)     or
        (not FileExists(fileName)) or
        (LowerCase(ExtractFileExt(fileName)) <> '.glsl'))
    then
    begin
        m_FragmentName := '';
        RecreateWnd;
        Exit;
    end;

    // update name and open file stream
    m_FragmentName := fileName;
    fileStream     := TFileStream.Create(m_FragmentName, fmOpenRead);

    try
        // copy fragment shader file content to memory
        m_pFragmentShader.CopyFrom(fileStream, fileStream.Size);
    finally
        fileStream.Free;
    end;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.DefineProperties(pFiler: TFiler);
    function DoWriteVertexShader: Boolean;
    begin
        if Assigned(pFiler.Ancestor) then
            Result := not (pFiler.Ancestor is TQRVCLShapeGL)
        else
            Result := m_pVertexShader.Size > 0;
    end;

    function DoWriteFragmentShader: Boolean;
    begin
        if Assigned(pFiler.Ancestor) then
            Result := not (pFiler.Ancestor is TQRVCLShapeGL)
        else
            Result := m_pFragmentShader.Size > 0;
    end;
begin
    inherited DefineProperties(pFiler);

    // register the properties that will load and save a binary data in DFM files
    pFiler.DefineBinaryProperty('VertexShader',   ReadVertexShader,   WriteVertexShader,   DoWriteVertexShader);
    pFiler.DefineBinaryProperty('FragmentShader', ReadFragmentShader, WriteFragmentShader, DoWriteFragmentShader);
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.ReadVertexShader(pStream: TStream);
begin
    // previous vertex shader was loaded?
    if (m_pVertexShader.Size > 0) then
        // clear it
        m_pVertexShader.Clear;

    // read vertex shader from DFM stream
    m_pVertexShader.CopyFrom(pStream, pStream.Size);
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.WriteVertexShader(pStream: TStream);
begin
    // reset stream position to start
    m_pVertexShader.Position := 0;

    // write vertex shader to DFM stream
    pStream.CopyFrom(m_pVertexShader, m_pVertexShader.Size);
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.ReadFragmentShader(pStream: TStream);
begin
    // previous fragment shader was loaded?
    if (m_pFragmentShader.Size > 0) then
        // clear it
        m_pFragmentShader.Clear;

    // read fragment shader from DFM stream
    m_pFragmentShader.CopyFrom(pStream, pStream.Size);
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.WriteFragmentShader(pStream: TStream);
begin
    // reset stream position to start
    m_pFragmentShader.Position := 0;

    // write fragment shader to DFM stream
    pStream.CopyFrom(m_pFragmentShader, m_pFragmentShader.Size);
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.CreateWindowHandle(const params: TCreateParams);
begin
    inherited CreateWindowHandle(params);

    // create a viewport
    CreateViewport(ClientWidth, ClientHeight);

    // (re)load model, done here because model is linked to current context. If context is killed,
    // some things in the model, as e.g. his texture, will become invalid and no more be painted
    // correctly
    LoadModel;
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.DestroyWindowHandle;
begin
    // as model is linked to the current context, clears it if context is shutting down
    if (Assigned(m_pShape)) then
        m_pShape.Clear;

    inherited DestroyWindowHandle;
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.OnAfterLoadModelEvent(const pGroup: TQRModelGroup);
begin
    // invalidate model to repaint it
    Invalidate();
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.OnTextureChanged(pSender: TObject);
begin
    RecreateWnd;
end;
//------------------------------------------------------------------------------
function TQRVCLShapeGL.OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                         const pModel: TQRModel;
                                              pBitmap: Vcl.Graphics.TBitmap;
                                             pTexture: TQRTexture;
                                         out loadNext: Boolean): Boolean;
var
    pixels:      TQRByteArray;
    pixelFormat: GLenum;
    hDC:         THandle;
    pSrcBitmap:  Vcl.Graphics.TBitmap;
begin
    // notify user that a texture should be loaded for the model
    if ((not(csDesigning in ComponentState)) and Assigned(m_fOnLoadTexture)) then
    begin
        Result := m_fOnLoadTexture(pGroup, pModel, pBitmap, pTexture, loadNext);
        Exit;
    end;

    // no model?
    if (not Assigned(pModel)) then
    begin
        Result := False;
        Exit;
    end;

    // no destination texture?
    if (not Assigned(pTexture)) then
    begin
        Result := False;
        Exit;
    end;

    // is source texture empty?
    if ((m_pTexture.Width = 0) or (m_pTexture.Height = 0)) then
    begin
        Result := False;
        Exit;
    end;

    // check if handle was successfully allocated
    if (not HandleAllocated) then
    begin
        Result := False;
        Exit;
    end;

    // get the device context for this control
    hDC := GetDC(WindowHandle);

    // found it?
    if (hDC = 0) then
    begin
        Result := False;
        Exit;
    end;

    try
        // enable OpenGL rendering context
        if (not m_pRenderSurface.EnableContext(hDC)) then
        begin
            Result := False;
            Exit;
        end;

        pSrcBitmap := Vcl.Graphics.TBitmap.Create;

        try
            // configure source bitmap and convert source texture to bitmap
            pSrcBitmap.PixelFormat := pf24bit;
            pSrcBitmap.SetSize(m_pTexture.Width, m_pTexture.Height);
            pSrcBitmap.Canvas.Draw(0, 0, m_pTexture.Graphic);

            // select pixel format to use
            pixelFormat := GL_RGB;

            try
                // convert bitmap to pixel array, and create OpenGL texture from array
                TQRVCLPictureHelper.BytesFromBitmap(pSrcBitmap, pixels, false, false);
                pTexture.Index := m_pRenderer.CreateTexture(pSrcBitmap.Width,
                                                            pSrcBitmap.Height,
                                                            pixelFormat,
                                                            pixels,
                                                            GL_NEAREST,
                                                            GL_NEAREST,
                                                            GL_TEXTURE_2D);
            finally
                SetLength(pixels, 0);
            end;
        finally
            pSrcBitmap.Free;
        end;
    finally
        ReleaseDC(WindowHandle, hDC);
    end;

    Result := True;
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.OnDrawSceneContent(hDC: Thandle);
begin
    if (not Assigned(m_pShape)) then
        Exit;

    // apply basic changes to model before drawing it
    m_pModel.Apply(m_pShape);

    try
        m_hSceneDC := hDC;

        // draw model
        m_pShape.Draw(0.0);
    finally
        m_hSceneDC := 0;
    end;
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.OnCustomDrawModelItem(const pGroup: TQRModelGroup;
                                                    pModel: TQRModel;
                                            const textures: TQRTextures;
                                              const matrix: TQRMatrix4x4);
begin
    if ((csDesigning in ComponentState) or not(Assigned(m_fDrawSceneStaticModelItemEvent))) then
        Exit;

    m_fDrawSceneStaticModelItemEvent(Self,
                                     m_hSceneDC,
                                     m_pRenderSurface.GLContext,
                                     m_pRenderer,
                                     m_pShader,
                                     pGroup,
                                     pModel,
                                     textures,
                                     matrix,
                                     nil,
                                     nil);
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.OnDrawModelItem(const pGroup: TQRModelGroup;
                                        const pModel: TQRModel;
                                      const textures: TQRTextures;
                                        const matrix: TQRMatrix4x4;
                                         const pMesh: PQRMesh;
                                     const pAABBTree: TQRAABBTree);
begin
    // notify user that model item is about to be drawn on the scene, stop drawing if user already
    // processed it
    if ((not(csDesigning in ComponentState)) and Assigned(m_fDrawSceneStaticModelItemEvent)) then
        if (m_fDrawSceneStaticModelItemEvent(Self,
                                             m_hSceneDC,
                                             m_pRenderSurface.GLContext,
                                             m_pRenderer,
                                             m_pShader,
                                             pGroup,
                                             pModel,
                                             textures,
                                             matrix,
                                             pMesh,
                                             pAABBTree))
        then
            Exit;

    // no model?
    if (not Assigned(pModel)) then
        Exit;

    // no mesh?
    if (not Assigned(pMesh)) then
        Exit;

    // draw mesh
    m_pRenderer.Draw(pMesh^, matrix, textures);

    // notify user that collisions may be detected
    if (Assigned(m_fOnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
        m_fOnDetectCollisions(Self, matrix, pAABBTree);
end;
//------------------------------------------------------------------------------
procedure TQRVCLShapeGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLShapeGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLShapeGL)) then
    begin
        // reset values to default
        m_ModelOptions := [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];
        m_VertexName   := '';
        m_FragmentName := '';

        m_pVertexShader.Clear;
        m_pFragmentShader.Clear;

        m_pModel.Assign(nil);
        m_pTexture.Assign(nil);
        Exit;
    end;

    // copy content from source
    pSrc           := pSource as TQRVCLShapeGL;
    m_ModelOptions := pSrc.m_ModelOptions;
    m_VertexName   := pSrc.m_VertexName;
    m_FragmentName := pSrc.m_FragmentName;

    m_pVertexShader.CopyFrom(pSrc.m_pVertexShader, pSrc.m_pVertexShader.Size);
    m_pFragmentShader.CopyFrom(pSrc.m_pFragmentShader, pSrc.m_pFragmentShader.Size);

    m_pModel.Assign(pSrc.m_pModel);
    m_pTexture.Assign(pSrc.m_pTexture);
end;
//------------------------------------------------------------------------------
// TQRVCLSurfaceGL
//------------------------------------------------------------------------------
constructor TQRVCLSurfaceGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize variables
    m_SurfaceWidth  := 2.0;
    m_SurfaceHeight := 2.0;

    // create surface
    m_pShape                       := TQRSurfaceGroup.Create;
    m_pShape.OnAfterLoadModelEvent := OnAfterLoadModelEvent;
    m_pShape.OnLoadMeshTexture     := OnLoadMeshTexture;
    m_pShape.OnDrawItem            := OnDrawModelItem;
    m_pShape.OnCustomDrawItem      := OnCustomDrawModelItem;
end;
//------------------------------------------------------------------------------
destructor TQRVCLSurfaceGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLSurfaceGL.SetSurfaceWidth(width: Single);
begin
    // no changes?
    if (m_SurfaceWidth = width) then
        Exit;

    m_SurfaceWidth := width;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLSurfaceGL.SetSurfaceHeight(height: Single);
begin
    // no changes?
    if (m_SurfaceHeight = height) then
        Exit;

    m_SurfaceHeight := height;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
function TQRVCLSurfaceGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
    begin
        Result := False;
        Exit;
    end;

    // is OpenGL context created?
    if (m_pRenderSurface.GLContext = 0) then
    begin
        Result := False;
        Exit;
    end;

    // is model a surface?
    if (not (m_pShape is TQRSurfaceGroup)) then
    begin
        Result := False;
        Exit;
    end;

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
        begin
            Result := False;
            Exit;
        end;
    end;

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pShape);

    // load model
    Result := TQRSurfaceGroup(m_pShape).Load(m_SurfaceWidth,
                                             m_SurfaceHeight,
                                             m_pModel.Color.NativeColor,
                                             m_ModelOptions);
end;
//------------------------------------------------------------------------------
procedure TQRVCLSurfaceGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLSurfaceGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLSurfaceGL)) then
    begin
        // reset values to default
        m_SurfaceWidth  := 2.0;
        m_SurfaceHeight := 2.0;
        Exit;
    end;

    // copy content from source
    pSrc            := pSource as TQRVCLSurfaceGL;
    m_SurfaceWidth  := pSrc.m_SurfaceWidth;
    m_SurfaceHeight := pSrc.m_SurfaceHeight;
end;
//------------------------------------------------------------------------------
// TQRVCLBoxGL
//------------------------------------------------------------------------------
constructor TQRVCLBoxGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize variables
    m_BoxWidth  := 1.0;
    m_BoxHeight := 1.0;
    m_BoxDepth  := 1.0;

    // create box
    m_pShape                       := TQRBoxGroup.Create;
    m_pShape.OnAfterLoadModelEvent := OnAfterLoadModelEvent;
    m_pShape.OnLoadMeshTexture     := OnLoadMeshTexture;
    m_pShape.OnDrawItem            := OnDrawModelItem;
    m_pShape.OnCustomDrawItem      := OnCustomDrawModelItem;
end;
//------------------------------------------------------------------------------
destructor TQRVCLBoxGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLBoxGL.SetBoxWidth(width: Single);
begin
    // no changes?
    if (m_BoxWidth = width) then
        Exit;

    m_BoxWidth := width;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLBoxGL.SetBoxHeight(height: Single);
begin
    // no changes?
    if (m_BoxHeight = height) then
        Exit;

    m_BoxHeight := height;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLBoxGL.SetBoxDepth(depth: Single);
begin
    // no changes?
    if (m_BoxDepth = depth) then
        Exit;

    m_BoxDepth := depth;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
function TQRVCLBoxGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
    begin
        Result := False;
        Exit;
    end;

    // is OpenGL context created?
    if (m_pRenderSurface.GLContext = 0) then
    begin
        Result := False;
        Exit;
    end;

    // is model a box?
    if (not (m_pShape is TQRBoxGroup)) then
    begin
        Result := False;
        Exit;
    end;

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
        begin
            Result := False;
            Exit;
        end;
    end;

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pShape);

    // load model
    Result := TQRBoxGroup(m_pShape).Load(m_BoxWidth,
                                         m_BoxHeight,
                                         m_BoxDepth,
                                         m_pModel.Color.NativeColor,
                                         m_RepeatTexOnEachFace,
                                         m_ModelOptions);
end;
//------------------------------------------------------------------------------
procedure TQRVCLBoxGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLBoxGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLBoxGL)) then
    begin
        // reset values to default
        m_BoxWidth  := 1.0;
        m_BoxHeight := 1.0;
        m_BoxDepth  := 1.0;
        Exit;
    end;

    // copy content from source
    pSrc        := pSource as TQRVCLBoxGL;
    m_BoxWidth  := pSrc.m_BoxWidth;
    m_BoxHeight := pSrc.m_BoxHeight;
    m_BoxDepth  := pSrc.m_BoxDepth;
end;
//------------------------------------------------------------------------------
// TQRVCLSphereGL
//------------------------------------------------------------------------------
constructor TQRVCLSphereGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize variables
    m_Slices  := 20;
    m_Stacks  := 20;
    m_Radius  := 1.0;

    // create box
    m_pShape                       := TQRSphereGroup.Create;
    m_pShape.OnAfterLoadModelEvent := OnAfterLoadModelEvent;
    m_pShape.OnLoadMeshTexture     := OnLoadMeshTexture;
    m_pShape.OnDrawItem            := OnDrawModelItem;
    m_pShape.OnCustomDrawItem      := OnCustomDrawModelItem;
end;
//------------------------------------------------------------------------------
destructor TQRVCLSphereGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLSphereGL.SetSlices(slices: NativeUInt);
begin
    // no changes?
    if (m_Slices = slices) then
        Exit;

    m_Slices := slices;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLSphereGL.SetStacks(stacks: NativeUInt);
begin
    // no changes?
    if (m_Stacks = stacks) then
        Exit;

    m_Stacks := stacks;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLSphereGL.SetRadius(radius: Single);
begin
    // no changes?
    if (m_Radius = radius) then
        Exit;

    m_Radius := radius;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
function TQRVCLSphereGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
    begin
        Result := False;
        Exit;
    end;

    // is OpenGL context created?
    if (m_pRenderSurface.GLContext = 0) then
    begin
        Result := False;
        Exit;
    end;

    // is model a sphere?
    if (not (m_pShape is TQRSphereGroup)) then
    begin
        Result := False;
        Exit;
    end;

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
        begin
            Result := False;
            Exit;
        end;
    end;

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pShape);

    // load model
    Result := TQRSphereGroup(m_pShape).Load(m_Slices,
                                            m_Stacks,
                                            m_Radius,
                                            m_pModel.Color.NativeColor,
                                            m_ModelOptions);
end;
//------------------------------------------------------------------------------
procedure TQRVCLSphereGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLSphereGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLSphereGL)) then
    begin
        // reset values to default
        m_Slices  := 20;
        m_Stacks  := 20;
        m_Radius  := 1.0;
        Exit;
    end;

    // copy content from source
    pSrc      := pSource as TQRVCLSphereGL;
    m_Slices  := pSrc.m_Slices;
    m_Stacks  := pSrc.m_Stacks;
    m_Radius  := pSrc.m_Radius;
end;
//------------------------------------------------------------------------------
// TQRVCLConeGL
//------------------------------------------------------------------------------
constructor TQRVCLConeGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize variables
    m_Faces         := 20;
    m_ConeHeight    := 1.5;
    m_TopRadiusX    := 1.0;
    m_TopRadiusY    := 1.0;
    m_BottomRadiusX := 1.0;
    m_BottomRadiusY := 1.0;
    m_Closing       := EQR_CC_Both;

    // create box
    m_pShape                       := TQRConeGroup.Create;
    m_pShape.OnAfterLoadModelEvent := OnAfterLoadModelEvent;
    m_pShape.OnLoadMeshTexture     := OnLoadMeshTexture;
    m_pShape.OnDrawItem            := OnDrawModelItem;
    m_pShape.OnCustomDrawItem      := OnCustomDrawModelItem;
end;
//------------------------------------------------------------------------------
destructor TQRVCLConeGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetFaces(faces: NativeUInt);
begin
    // no changes?
    if (m_Faces = faces) then
        Exit;

    m_Faces := faces;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetConeHeight(height: Single);
begin
    // no changes?
    if (m_ConeHeight = height) then
        Exit;

    m_ConeHeight := height;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetTopRadiusX(topRadiusX: Single);
begin
    // no changes?
    if (m_TopRadiusX = topRadiusX) then
        Exit;

    m_TopRadiusX := topRadiusX;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetTopRadiusY(topRadiusY: Single);
begin
    // no changes?
    if (m_TopRadiusY = topRadiusY) then
        Exit;

    m_TopRadiusY := topRadiusY;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetBottomRadiusX(bottomRadiusX: Single);
begin
    // no changes?
    if (m_BottomRadiusX = bottomRadiusX) then
        Exit;

    m_BottomRadiusX := bottomRadiusX;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetBottomRadiusY(bottomRadiusY: Single);
begin
    // no changes?
    if (m_BottomRadiusY = bottomRadiusY) then
        Exit;

    m_BottomRadiusY := bottomRadiusY;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetClosing(closing: EQR_Cone_Closing);
begin
    // no changes?
    if (m_Closing = closing) then
        Exit;

    m_Closing := closing;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
function TQRVCLConeGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
    begin
        Result := False;
        Exit;
    end;

    // is OpenGL context created?
    if (m_pRenderSurface.GLContext = 0) then
    begin
        Result := False;
        Exit;
    end;

    // is model a cone?
    if (not (m_pShape is TQRConeGroup)) then
    begin
        Result := False;
        Exit;
    end;

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
        begin
            Result := False;
            Exit;
        end;
    end;

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pShape);

    // load model
    Result := TQRConeGroup(m_pShape).Load(m_Faces,
                                          m_ConeHeight,
                                          m_TopRadiusX,
                                          m_TopRadiusY,
                                          m_BottomRadiusX,
                                          m_BottomRadiusY,
                                          m_Closing,
                                          m_pModel.Color.NativeColor,
                                          m_ModelOptions);
end;
//------------------------------------------------------------------------------
procedure TQRVCLConeGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLConeGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLConeGL)) then
    begin
        // reset values to default
        m_Faces         := 20;
        m_ConeHeight    := 1.5;
        m_TopRadiusX    := 1.0;
        m_TopRadiusY    := 1.0;
        m_BottomRadiusX := 1.0;
        m_BottomRadiusY := 1.0;
        m_Closing       := EQR_CC_Both;
        Exit;
    end;

    // copy content from source
    pSrc            := pSource as TQRVCLConeGL;
    m_Faces         := pSrc.m_Faces;
    m_ConeHeight    := pSrc.m_ConeHeight;
    m_TopRadiusX    := pSrc.m_TopRadiusX;
    m_TopRadiusY    := pSrc.m_TopRadiusY;
    m_BottomRadiusX := pSrc.m_BottomRadiusX;
    m_BottomRadiusY := pSrc.m_BottomRadiusY;
    m_Closing       := pSrc.m_Closing;
end;
//------------------------------------------------------------------------------
// TQRVCLTorusGL
//------------------------------------------------------------------------------
constructor TQRVCLTorusGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize variables
    m_Slices         := 20;
    m_FacesPerSlices := 20;
    m_OuterRadiusX   := 0.2;
    m_OuterRadiusY   := 0.2;
    m_InnerRadiusX   := 0.8;
    m_InnerRadiusY   := 0.8;

    // create box
    m_pShape                       := TQRTorusGroup.Create;
    m_pShape.OnAfterLoadModelEvent := OnAfterLoadModelEvent;
    m_pShape.OnLoadMeshTexture     := OnLoadMeshTexture;
    m_pShape.OnDrawItem            := OnDrawModelItem;
    m_pShape.OnCustomDrawItem      := OnCustomDrawModelItem;
end;
//------------------------------------------------------------------------------
destructor TQRVCLTorusGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetSlices(slices: NativeUInt);
begin
    // no changes?
    if (m_Slices = slices) then
        Exit;

    m_Slices := slices;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetFacesPerSlices(facesPerSlices: NativeUInt);
begin
    // no changes?
    if (m_FacesPerSlices = facesPerSlices) then
        Exit;

    m_FacesPerSlices := facesPerSlices;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetInnerRadiusX(innerRadiusX: Single);
begin
    // no changes?
    if (m_InnerRadiusX = innerRadiusX) then
        Exit;

    m_InnerRadiusX := innerRadiusX;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetInnerRadiusY(innerRadiusY: Single);
begin
    // no changes?
    if (m_InnerRadiusY = innerRadiusY) then
        Exit;

    m_InnerRadiusY := innerRadiusY;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetOuterRadiusX(outerRadiusX: Single);
begin
    // no changes?
    if (m_OuterRadiusX = outerRadiusX) then
        Exit;

    m_OuterRadiusX := outerRadiusX;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetOuterRadiusY(outerRadiusY: Single);
begin
    // no changes?
    if (m_OuterRadiusY = outerRadiusY) then
        Exit;

    m_OuterRadiusY := outerRadiusY;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
function TQRVCLTorusGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
    begin
        Result := False;
        Exit;
    end;

    // is OpenGL context created?
    if (m_pRenderSurface.GLContext = 0) then
    begin
        Result := False;
        Exit;
    end;

    // is model a sphere?
    if (not (m_pShape is TQRTorusGroup)) then
    begin
        Result := False;
        Exit;
    end;

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
        begin
            Result := False;
            Exit;
        end;
    end;

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pShape);

    // load model
    Result := TQRTorusGroup(m_pShape).Load(m_Slices,
                                           m_FacesPerSlices,
                                           m_InnerRadiusX,
                                           m_InnerRadiusY,
                                           m_OuterRadiusX,
                                           m_OuterRadiusY,
                                           m_pModel.Color.NativeColor,
                                           m_ModelOptions);
end;
//------------------------------------------------------------------------------
procedure TQRVCLTorusGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLTorusGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLTorusGL)) then
    begin
        // reset values to default
        m_Slices         := 20;
        m_FacesPerSlices := 20;
        m_OuterRadiusX   := 0.2;
        m_OuterRadiusY   := 0.2;
        m_InnerRadiusX   := 0.8;
        m_InnerRadiusY   := 0.8;
        Exit;
    end;

    // copy content from source
    pSrc             := pSource as TQRVCLTorusGL;
    m_Slices         := pSrc.m_Slices;
    m_FacesPerSlices := pSrc.m_FacesPerSlices;
    m_OuterRadiusX   := pSrc.m_OuterRadiusX;
    m_OuterRadiusY   := pSrc.m_OuterRadiusY;
    m_InnerRadiusX   := pSrc.m_InnerRadiusX;
    m_InnerRadiusY   := pSrc.m_InnerRadiusY;
end;
//------------------------------------------------------------------------------
// TQRVCLParabolaGL
//------------------------------------------------------------------------------
constructor TQRVCLParabolaGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize variables
    m_Slices         := 20;
    m_FacesPerSlices := 20;
    m_ParabolaHeight := 1.0;
    m_Radius         := 1.0;

    // create box
    m_pShape                       := TQRParabolaGroup.Create;
    m_pShape.OnAfterLoadModelEvent := OnAfterLoadModelEvent;
    m_pShape.OnLoadMeshTexture     := OnLoadMeshTexture;
    m_pShape.OnDrawItem            := OnDrawModelItem;
    m_pShape.OnCustomDrawItem      := OnCustomDrawModelItem;
end;
//------------------------------------------------------------------------------
destructor TQRVCLParabolaGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLParabolaGL.SetSlices(slices: NativeUInt);
begin
    // no changes?
    if (m_Slices = slices) then
        Exit;

    m_Slices := slices;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLParabolaGL.SetFacesPerSlices(facesPerSlices: NativeUInt);
begin
    // no changes?
    if (m_FacesPerSlices = facesPerSlices) then
        Exit;

    m_FacesPerSlices := facesPerSlices;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLParabolaGL.SetParabolaHeight(height: Single);
begin
    // no changes?
    if (m_ParabolaHeight = height) then
        Exit;

    m_ParabolaHeight := height;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
procedure TQRVCLParabolaGL.SetRadius(radius: Single);
begin
    // no changes?
    if (m_Radius = radius) then
        Exit;

    m_Radius := Radius;

    RecreateWnd;
end;
//------------------------------------------------------------------------------
function TQRVCLParabolaGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
    begin
        Result := False;
        Exit;
    end;

    // is OpenGL context created?
    if (m_pRenderSurface.GLContext = 0) then
    begin
        Result := False;
        Exit;
    end;

    // is model a parabola?
    if (not (m_pShape is TQRParabolaGroup)) then
    begin
        Result := False;
        Exit;
    end;

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
        begin
            Result := False;
            Exit;
        end;
    end;

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pShape);

    // load model
    Result := TQRParabolaGroup(m_pShape).Load(m_Slices,
                                              m_FacesPerSlices,
                                              m_ParabolaHeight,
                                              m_Radius,
                                              m_pModel.Color.NativeColor,
                                              m_ModelOptions);
end;
//------------------------------------------------------------------------------
procedure TQRVCLParabolaGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLParabolaGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLParabolaGL)) then
    begin
        // reset values to default
        m_Slices         := 20;
        m_FacesPerSlices := 20;
        m_ParabolaHeight := 1.0;
        m_Radius         := 1.0;
        Exit;
    end;

    // copy content from source
    pSrc             := pSource as TQRVCLParabolaGL;
    m_Slices         := pSrc.m_Slices;
    m_FacesPerSlices := pSrc.m_FacesPerSlices;
    m_ParabolaHeight := pSrc.m_ParabolaHeight;
    m_Radius         := pSrc.m_Radius;
end;
//------------------------------------------------------------------------------

end.
