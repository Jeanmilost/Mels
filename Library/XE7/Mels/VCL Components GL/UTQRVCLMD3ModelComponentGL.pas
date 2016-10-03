{**************************************************************************************************
 * ==> UTQRVCLMD3ModelComponentGL ----------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides a MD3 model component using the VCL and OpenGL to draw it   *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLMD3ModelComponentGL;

interface
    // do not include XE7.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE XE7.OpenGLext *)

uses System.Classes,
     System.SysUtils,
     UTQRHelpers,
     UTQRVCLHelpers,
     UTQRLogging,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRModel,
     UTQRMD3,
     UTQRModelGroup,
     UTQRMD3ModelGroup,
     UTQRVCLModelComponentGL,
     UTQRVCLModelComponentPropertiesGL,
     Vcl.Graphics,
     Vcl.Controls,
     Winapi.Messages,
     Winapi.Windows,
     // unfortunately the required OpenGL headers does not exist or are incomplete in XE4 and
     // earlier, so the DelphiGL component (provided with installation) should be used instead
     XE7.OpenGL, XE7.OpenGLext;

type
    {**
    * MD3 torso animations
    *}
    EQRMD3AnimationTorsoGesture =
    (
        EQR_AT_MD3_Death1,
        EQR_AT_MD3_Dead1,
        EQR_AT_MD3_Death2,
        EQR_AT_MD3_Dead2,
        EQR_AT_MD3_Death3,
        EQR_AT_MD3_Dead3,
        EQR_AT_MD3_Gesture,
        EQR_AT_MD3_Attack,
        EQR_AT_MD3_Attack2,
        EQR_AT_MD3_Drop,
        EQR_AT_MD3_Raise,
        EQR_AT_MD3_Stand,
        EQR_AT_MD3_Stand2
    );

    {**
    * MD3 legs animations
    *}
    EQRMD3AnimationLegsGesture =
    (
        EQR_AL_MD3_Death1,
        EQR_AL_MD3_Dead1,
        EQR_AL_MD3_Death2,
        EQR_AL_MD3_Dead2,
        EQR_AL_MD3_Death3,
        EQR_AL_MD3_Dead3,
        EQR_AL_MD3_Walk_Crouching,
        EQR_AL_MD3_Walk,
        EQR_AL_MD3_Run,
        EQR_AL_MD3_Back,
        EQR_AL_MD3_Swim,
        EQR_AL_MD3_Jump,
        EQR_AL_MD3_Land,
        EQR_AL_MD3_Jump_Back,
        EQR_AL_MD3_Land_Back,
        EQR_AL_MD3_Idle,
        EQR_AL_MD3_Idle_Crouching,
        EQR_AL_MD3_Turn
    );

    {**
    * MD3 model component
    *}
    TQRVCLMD3ModelGL = class(TQRVCLFramedModelComponentGL)
        protected
            m_pMD3:               TQRMD3Group;
            m_pModel:             TQRVCLModelComponentPropertyGL;
            m_pPackage:           TMemoryStream;
            m_pVertexShader:      TMemoryStream;
            m_pFragmentShader:    TMemoryStream;
            m_VertexName:         TFileName;
            m_FragmentName:       TFileName;
            m_PackageName:        TFileName;
            m_TorsoGesture:       EQRMD3AnimationTorsoGesture;
            m_LegsGesture:        EQRMD3AnimationLegsGesture;
            m_ModelOptions:       TQRModelOptions;
            m_FramedModelOptions: TQRFramedModelOptions;
            m_hSceneDC:           THandle;
            m_AnimationGesture:   EQRMD3AnimationGesture;

            {**
            * Sets model package file name
            *@param fileName - file name
            *}
            procedure SetPackageName(fileName: TFileName); virtual;

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
            * Sets torso gesture
            *@param gesture - torso gesture
            *}
            procedure SetTorsoGesture(gesture: EQRMD3AnimationTorsoGesture); virtual;

            {**
            * Sets legs gesture
            *@param gesture - legs gesture
            *}
            procedure SetLegsGesture(gesture: EQRMD3AnimationLegsGesture); virtual;

            {**
            * Sets model options
            *@param options - options
            *}
            procedure SetModelOptions(options: TQRModelOptions); virtual;

            {**
            * Sets framed model options
            *@param options - options
            *}
            procedure SetFramedModelOptions(options: TQRFramedModelOptions); virtual;

            {**
            * Sets legs gesture to model
            *}
            procedure SetTorsoGestureToModel; virtual;

            {**
            * Sets legs gesture to model
            *}
            procedure SetLegsGestureToModel; virtual;

            {**
            * Declares properties that will deal with DFM files
            *@param pFiler - DFM file manager
            *}
            procedure DefineProperties(pFiler: TFiler); override;

            {**
            * Reads package content from DFM file
            *@param pStream - stream containing DFM data
            *}
            procedure ReadPackage(pStream: TStream); virtual;

            {**
            * Writes package content to DFM file
            *@param pStream - DFM stream in which package should be written
            *}
            procedure WritePackage(pStream: TStream); virtual;

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
            * Called after control was fully loaded from DFM stream
            *}
            procedure Loaded; override;

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
            * Creates a viewport for the component
            *@param width - viewport width
            *@param height - viewport height
            *}
            procedure CreateViewport(width, height: NativeUInt); override;

            {**
            * Loads the model
            *@return true on success, otherwise false
            *}
            function LoadModel: Boolean; virtual;

            {**
            * Prepares shader to be used while scene is drawn
            *@param textures - model textures
            *}
            function PrepareShaderToDrawModel(const textures: TQRTextures): Boolean; virtual;

            {**
            * Called after model was completely loaded
            *@param pGroup - group that finished to load the model
            *}
            procedure OnAfterLoadModelEvent(const pGroup: TQRModelGroup); virtual;

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
            *@param index - model mesh index
            *@param nextIndex - model mesh index to interpolate with
            *@param interpolationFactor - interpolation factor
            *}
            procedure OnCustomDrawModelItem(const pGroup: TQRModelGroup;
                                                  pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4;
                                        index, nextIndex: NativeInt;
                               const interpolationFactor: Double); virtual;

            {**
            * Called when framed model item should be drawn
            *@param pGroup - group at which model belongs
            *@param pModel - model to draw
            *@param textures - textures belonging to model, in the order where they should be combined
            *@param matrix - model matrix
            *@param index - model mesh index
            *@param nextIndex - model mesh index to interpolate with
            *@param interpolationFactor - interpolation factor
            *@param pMesh - mesh to draw, can be NULL
            *@param pNextMesh - next mesh to interpolate with, can be NULL
            *@param pAABBTree - aligned-axis bounding box tree matching with mesh, can be NULL
            *@param pNextAABBTree - aligned-axis bounding box tree matching with next mesh, can be NULL
            *}
            procedure OnDrawModelItem(const pGroup: TQRModelGroup;
                                      const pModel: TQRModel;
                                    const textures: TQRTextures;
                                      const matrix: TQRMatrix4x4;
                                  index, nextIndex: NativeInt;
                         const interpolationFactor: Double;
                            const pMesh, pNextMesh: PQRMesh;
                    const pAABBTree, pNextAABBTree: TQRAABBTree); virtual;

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
            property Model:              TQRVCLModelComponentPropertyGL read m_pModel             write m_pModel;
            property PackageName:        TFileName                      read m_PackageName        write SetPackageName;
            property VertexName:         TFileName                      read m_VertexName         write SetVertexName;
            property FragmentName:       TFileName                      read m_FragmentName       write SetFragmentName;
            property TorsoGesture:       EQRMD3AnimationTorsoGesture    read m_TorsoGesture       write SetTorsoGesture       default EQR_AT_MD3_Stand;
            property LegsGesture:        EQRMD3AnimationLegsGesture     read m_LegsGesture        write SetLegsGesture        default EQR_AL_MD3_Idle;
            property ModelOptions:       TQRModelOptions                read m_ModelOptions       write SetModelOptions       default [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];
            property FramedModelOptions: TQRFramedModelOptions          read m_FramedModelOptions write SetFramedModelOptions;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLMD3ModelGL
//--------------------------------------------------------------------------------------------------
constructor TQRVCLMD3ModelGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // override basic data, to enable double-buffering on OpenGL rendering (means also that embedded
    // GDI rendering isn't supported for this component)
    m_SupportsGDI := False;

    // initialize variables
    m_pMD3               := TQRMD3Group.Create;
    m_pModel             := TQRVCLModelComponentPropertyGL.Create(Self, OnReceivePropNotification);
    m_pPackage           := TMemoryStream.Create;
    m_pVertexShader      := TMemoryStream.Create;
    m_pFragmentShader    := TMemoryStream.Create;
    m_TorsoGesture       := EQR_AT_MD3_Stand;
    m_LegsGesture        := EQR_AL_MD3_Idle;
    m_ModelOptions       := [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];
    m_FramedModelOptions := [];
    m_hSceneDC           := 0;

    // configure model
    m_pMD3.OnAfterLoadModelEvent := OnAfterLoadModelEvent;
    m_pMD3.OnLoadMeshTexture     := OnLoadMeshTexture;
    m_pMD3.OnCustomDrawItem      := OnCustomDrawModelItem;
    m_pMD3.OnDrawItem            := OnDrawModelItem;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLMD3ModelGL.Destroy;
begin
    // clear memory
    m_pFragmentShader.Free;
    m_pVertexShader.Free;
    m_pPackage.Free;
    m_pModel.Free;
    m_pMD3.Free;

    // set explicitly the MD3 model to nil after deleted it, because in some situations it can be
    // accessed later in destruction, e.g. when handle is destroyed after a control was deleted from
    // designer
    m_pMD3 := nil;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.SetPackageName(fileName: TFileName);
var
    fileStream: TFileStream;
begin
    // no changes?
    if (fileName = m_PackageName) then
        Exit;

    // if component is currently loading, just update the property name, the model will be loaded
    // during the serialization
    if (csLoading in ComponentState) then
    begin
        m_PackageName := fileName;
        Exit;
    end;

    // previous package was loaded?
    if (m_pPackage.Size > 0) then
        // clear it
        m_pPackage.Clear;

    // file name isn't empty, file exists and is a MD3 package file?
    if ((Length(fileName) = 0)     or
        (not FileExists(fileName)) or
        (LowerCase(ExtractFileExt(fileName)) <> '.pk3'))
    then
    begin
        // clear previously loaded model
        m_PackageName := '';
        m_pMD3.Clear;
        Exit;
    end;

    // update name and open file stream
    m_PackageName := fileName;
    fileStream    := TFileStream.Create(m_PackageName, fmOpenRead);

    try
        // copy package file content to memory
        m_pPackage.CopyFrom(fileStream, fileStream.Size);
    finally
        fileStream.Free;
    end;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.SetVertexName(fileName: TFileName);
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
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.SetFragmentName(fileName: TFileName);
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
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.SetTorsoGesture(gesture: EQRMD3AnimationTorsoGesture);
begin
    // no changes?
    if (m_TorsoGesture = gesture) then
        Exit;

    m_TorsoGesture := gesture;

    SetTorsoGestureToModel;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.SetLegsGesture(gesture: EQRMD3AnimationLegsGesture);
begin
    // no changes?
    if (m_LegsGesture = gesture) then
        Exit;

    m_LegsGesture := gesture;

    SetLegsGestureToModel;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.SetModelOptions(options: TQRModelOptions);
begin
    // no changes?
    if (m_ModelOptions = options) then
        Exit;

    m_ModelOptions := options;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.SetFramedModelOptions(options: TQRFramedModelOptions);
begin
    // no changes?
    if (m_FramedModelOptions = options) then
        Exit;

    m_FramedModelOptions := options;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.SetTorsoGestureToModel;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        Exit;

    // set torso gesture to run
    case (m_TorsoGesture) of
        EQR_AT_MD3_Death1:  m_pMD3.SetAnimation('upper', EQR_AG_MD3_Both_Death1);
        EQR_AT_MD3_Dead1:   m_pMD3.SetAnimation('upper', EQR_AG_MD3_Both_Dead1);
        EQR_AT_MD3_Death2:  m_pMD3.SetAnimation('upper', EQR_AG_MD3_Both_Death2);
        EQR_AT_MD3_Dead2:   m_pMD3.SetAnimation('upper', EQR_AG_MD3_Both_Dead2);
        EQR_AT_MD3_Death3:  m_pMD3.SetAnimation('upper', EQR_AG_MD3_Both_Death3);
        EQR_AT_MD3_Dead3:   m_pMD3.SetAnimation('upper', EQR_AG_MD3_Both_Dead3);
        EQR_AT_MD3_Gesture: m_pMD3.SetAnimation('upper', EQR_AG_MD3_Torso_Gesture);
        EQR_AT_MD3_Attack:  m_pMD3.SetAnimation('upper', EQR_AG_MD3_Torso_Attack);
        EQR_AT_MD3_Attack2: m_pMD3.SetAnimation('upper', EQR_AG_MD3_Torso_Attack2);
        EQR_AT_MD3_Drop:    m_pMD3.SetAnimation('upper', EQR_AG_MD3_Torso_Drop);
        EQR_AT_MD3_Raise:   m_pMD3.SetAnimation('upper', EQR_AG_MD3_Torso_Raise);
        EQR_AT_MD3_Stand:   m_pMD3.SetAnimation('upper', EQR_AG_MD3_Torso_Stand);
        EQR_AT_MD3_Stand2:  m_pMD3.SetAnimation('upper', EQR_AG_MD3_Torso_Stand2);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.SetLegsGestureToModel;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        Exit;

    // set legs gesture to run
    case (m_LegsGesture) of
        EQR_AL_MD3_Death1:         m_pMD3.SetAnimation('lower', EQR_AG_MD3_Both_Death1);
        EQR_AL_MD3_Dead1:          m_pMD3.SetAnimation('lower', EQR_AG_MD3_Both_Dead1);
        EQR_AL_MD3_Death2:         m_pMD3.SetAnimation('lower', EQR_AG_MD3_Both_Death2);
        EQR_AL_MD3_Dead2:          m_pMD3.SetAnimation('lower', EQR_AG_MD3_Both_Dead2);
        EQR_AL_MD3_Death3:         m_pMD3.SetAnimation('lower', EQR_AG_MD3_Both_Death3);
        EQR_AL_MD3_Dead3:          m_pMD3.SetAnimation('lower', EQR_AG_MD3_Both_Dead3);
        EQR_AL_MD3_Walk_Crouching: m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Walk_Crouching);
        EQR_AL_MD3_Walk:           m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Walk);
        EQR_AL_MD3_Run:            m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Run);
        EQR_AL_MD3_Back:           m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Back);
        EQR_AL_MD3_Swim:           m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Swim);
        EQR_AL_MD3_Jump:           m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Jump);
        EQR_AL_MD3_Land:           m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Land);
        EQR_AL_MD3_Jump_Back:      m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Jump_Back);
        EQR_AL_MD3_Land_Back:      m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Land_Back);
        EQR_AL_MD3_Idle:           m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Idle);
        EQR_AL_MD3_Idle_Crouching: m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Idle_Crouching);
        EQR_AL_MD3_Turn:           m_pMD3.SetAnimation('lower', EQR_AG_MD3_Legs_Turn);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.DefineProperties(pFiler: TFiler);
    function DoWritePackage: Boolean;
    begin
        if Assigned(pFiler.Ancestor) then
            Result := not (pFiler.Ancestor is TQRVCLMD3ModelGL)
        else
            Result := m_pPackage.Size > 0;
    end;

    function DoWriteVertexShader: Boolean;
    begin
        if Assigned(pFiler.Ancestor) then
            Result := not (pFiler.Ancestor is TQRVCLMD3ModelGL)
        else
            Result := m_pVertexShader.Size > 0;
    end;

    function DoWriteFragmentShader: Boolean;
    begin
        if Assigned(pFiler.Ancestor) then
            Result := not (pFiler.Ancestor is TQRVCLMD3ModelGL)
        else
            Result := m_pFragmentShader.Size > 0;
    end;
begin
    inherited DefineProperties(pFiler);

    // register the properties that will load and save a binary data in DFM files
    pFiler.DefineBinaryProperty('Package',        ReadPackage,        WritePackage,        DoWritePackage);
    pFiler.DefineBinaryProperty('VertexShader',   ReadVertexShader,   WriteVertexShader,   DoWriteVertexShader);
    pFiler.DefineBinaryProperty('FragmentShader', ReadFragmentShader, WriteFragmentShader, DoWriteFragmentShader);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.ReadPackage(pStream: TStream);
begin
    // previous package was loaded?
    if (m_pPackage.Size > 0) then
        // clear it
        m_pPackage.Clear;

    // read model package from DFM stream
    m_pPackage.CopyFrom(pStream, pStream.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.WritePackage(pStream: TStream);
begin
    // reset stream position to start
    m_pPackage.Position := 0;

    // write model package to DFM stream
    pStream.CopyFrom(m_pPackage, m_pPackage.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.ReadVertexShader(pStream: TStream);
begin
    // previous vertex shader was loaded?
    if (m_pVertexShader.Size > 0) then
        // clear it
        m_pVertexShader.Clear;

    // read vertex shader from DFM stream
    m_pVertexShader.CopyFrom(pStream, pStream.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.WriteVertexShader(pStream: TStream);
begin
    // reset stream position to start
    m_pVertexShader.Position := 0;

    // write vertex shader to DFM stream
    pStream.CopyFrom(m_pVertexShader, m_pVertexShader.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.ReadFragmentShader(pStream: TStream);
begin
    // previous fragment shader was loaded?
    if (m_pFragmentShader.Size > 0) then
        // clear it
        m_pFragmentShader.Clear;

    // read fragment shader from DFM stream
    m_pFragmentShader.CopyFrom(pStream, pStream.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.WriteFragmentShader(pStream: TStream);
begin
    // reset stream position to start
    m_pFragmentShader.Position := 0;

    // write fragment shader to DFM stream
    pStream.CopyFrom(m_pFragmentShader, m_pFragmentShader.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.Loaded;
begin
    inherited Loaded;

    // this situation may occur after a new component was created on the designer, e.g. after a
    // copy/paste operation. In this case, the model may fail to load, although the OpenGL context
    // is initialized
    if ((csDesigning in ComponentState) and
         HandleAllocated                and
         Assigned(m_pMD3)               and
         m_pMD3.IsEmpty                 and
        (m_pPackage.Size > 0))
    then
    begin
        RecreateWnd;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.CreateWindowHandle(const params: TCreateParams);
begin
    inherited CreateWindowHandle(params);

    // create a viewport
    CreateViewport(ClientWidth, ClientHeight);

    // (re)load model, done here because model is linked to current context. If context is killed,
    // some things in the model, as e.g. his texture, will become invalid and no more be painted
    // correctly
    LoadModel;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.DestroyWindowHandle;
begin
    // as model is linked to the current context, clears it if context is shutting down
    if (Assigned(m_pMD3)) then
        m_pMD3.Clear;

    inherited DestroyWindowHandle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.CreateViewport(width, height: NativeUInt);
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

        // get antialiasing factor to apply
        factor := GetAntialiasingFactor;

        // create OpenGL viewport to use to draw scene
        m_pRenderer.CreateViewport(ClientWidth * factor, ClientHeight * factor);

        // notify user that scene matrix (i.e. projection and view matrix) are about to be created
        if (Assigned(m_fOnCreateSceneMatrix)) then
            // user defined his own matrix?
            if (m_fOnCreateSceneMatrix(Self, m_ProjectionMatrix, m_ViewMatrix)) then
                Exit;

        // create projection matrix
        m_ProjectionMatrix := m_pRenderer.GetProjection(45.0,
                                                        ClientWidth  * factor,
                                                        ClientHeight * factor,
                                                        1.0,
                                                        1000.0);

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
    finally
        ReleaseDC(WindowHandle, hDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLMD3ModelGL.LoadModel: Boolean;
var
    pPackage: TMemoryStream;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
    begin
        Result := False;
        Exit;
    end;

    // no model to load?
    if (m_pPackage.Size <= 0) then
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

    // reset stream position to start
    m_pPackage.Position := 0;

    pPackage := nil;

    try
        pPackage := TMemoryStream.Create;

        // copy package to load inside a local stream, because the MD3 group will take the
        // ownership of the stream pointer
        pPackage.CopyFrom(m_pPackage, m_pPackage.Size);
    except
        on e: Exception do
        begin
            // clear memory
            pPackage.Free;

            Result := False;
            Exit;
        end;
    end;

    // reset stream position to start
    pPackage.Position := 0;

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pMD3);

    // load model
    if (not m_pMD3.Load(pPackage,
                        m_pModel.Color.NativeColor,
                        False,
                        m_ModelOptions,
                        m_FramedModelOptions))
    then
    begin
        Result := False;
        Exit;
    end;

    // set gestures to run
    SetTorsoGestureToModel;
    SetLegsGestureToModel;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLMD3ModelGL.PrepareShaderToDrawModel(const textures: TQRTextures): Boolean;
var
    uniform: GLint;
begin
    // OpenGL was not initialized correctly?
    if (not Allowed) then
    begin
        Result := False;
        Exit;
    end;

    // bind shader program
    m_pShader.Use(True);

    // get perspective (or projection) matrix slot from shader
    uniform := m_pRenderer.GetUniform(m_pShader, EQR_SA_PerspectiveMatrix);

    // found it?
    if (uniform = -1) then
    begin
        TQRLogHelper.LogToCompiler('Program uniform not found - perspective');
        Result := False;
        Exit;
    end;

    // connect perspective (or projection) matrix to shader
    glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(m_ProjectionMatrix.GetPtr));

    // get view (or camera) matrix slot from shader
    uniform := m_pRenderer.GetUniform(m_pShader, EQR_SA_CameraMatrix);

    // found it?
    if (uniform = -1) then
    begin
        TQRLogHelper.LogToCompiler('Program uniform not found - camera');
        Result := False;
        Exit;
    end;

    // connect view (or camera) matrix to shader
    glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(m_ViewMatrix.GetPtr));

    // unbind shader program
    m_pShader.Use(False);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.OnAfterLoadModelEvent(const pGroup: TQRModelGroup);
begin
    // is component in design time?
    if (m_NoAnimation or (csDesigning in ComponentState)) then
        // invalidate model to repaint it, as in design time animation isn't running
        Invalidate();
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLMD3ModelGL.OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                            const pModel: TQRModel;
                                                 pBitmap: Vcl.Graphics.TBitmap;
                                                pTexture: TQRTexture;
                                            out loadNext: Boolean): Boolean;
var
    pixels:      TQRByteArray;
    pixelFormat: GLenum;
    hDC:         THandle;
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

    // no texture?
    if (not Assigned(pTexture)) then
    begin
        Result := False;
        Exit;
    end;

    // no texture bitmap?
    if (not Assigned(pBitmap)) then
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

        // select pixel format to use
        if (pBitmap.PixelFormat = pf32bit) then
            pixelFormat := GL_RGBA
        else
            pixelFormat := GL_RGB;

        try
            // convert bitmap to pixel array, and create OpenGL texture from array
            TQRVCLPictureHelper.BytesFromBitmap(pBitmap, pixels, false, false);
            pTexture.Index := m_pRenderer.CreateTexture(pBitmap.Width,
                                                        pBitmap.Height,
                                                        pixelFormat,
                                                        pixels,
                                                        GL_NEAREST,
                                                        GL_NEAREST,
                                                        GL_TEXTURE_2D);
        finally
            SetLength(pixels, 0);
        end;
    finally
        ReleaseDC(WindowHandle, hDC);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.OnDrawSceneContent(hDC: THandle);
begin
    // apply basic changes to model before drawing it
    m_pModel.Apply(m_pMD3);

    try
        m_hSceneDC := hDC;

        // draw model
        m_pMD3.Draw(m_ElapsedTime);
    finally
        m_hSceneDC := 0;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.OnCustomDrawModelItem(const pGroup: TQRModelGroup;
                                                       pModel: TQRModel;
                                               const textures: TQRTextures;
                                                 const matrix: TQRMatrix4x4;
                                             index, nextIndex: NativeInt;
                                    const interpolationFactor: Double);
begin
    if ((csDesigning in ComponentState) or not(Assigned(m_fDrawSceneFramedModelItemEvent))) then
        Exit;

    m_fDrawSceneFramedModelItemEvent(Self,
                                     m_hSceneDC,
                                     m_pRenderSurface.GLContext,
                                     m_pRenderer,
                                     m_pShader,
                                     pGroup,
                                     pModel,
                                     textures,
                                     matrix,
                                     index,
                                     nextIndex,
                                     interpolationFactor,
                                     nil,
                                     nil,
                                     nil,
                                     nil);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.OnDrawModelItem(const pGroup: TQRModelGroup;
                                           const pModel: TQRModel;
                                         const textures: TQRTextures;
                                           const matrix: TQRMatrix4x4;
                                       index, nextIndex: NativeInt;
                              const interpolationFactor: Double;
                                 const pMesh, pNextMesh: PQRMesh;
                         const pAABBTree, pNextAABBTree: TQRAABBTree);
var
    mesh: TQRMesh;
begin
    // notify user that model item is about to be drawn on the scene, stop drawing if user already
    // processed it
    if ((not(csDesigning in ComponentState)) and Assigned(m_fDrawSceneFramedModelItemEvent)) then
        if (m_fDrawSceneFramedModelItemEvent(Self,
                                             m_hSceneDC,
                                             m_pRenderSurface.GLContext,
                                             m_pRenderer,
                                             m_pShader,
                                             pGroup,
                                             pModel,
                                             textures,
                                             matrix,
                                             index,
                                             nextIndex,
                                             interpolationFactor,
                                             pMesh,
                                             pNextMesh,
                                             pAABBTree,
                                             pNextAABBTree))
        then
            Exit;

    // no model?
    if (not Assigned(pModel)) then
        Exit;

    // no mesh?
    if (not Assigned(pMesh)) then
        Exit;

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // prepare shader to draw the model
        PrepareShaderToDrawModel(textures);

        // draw mesh
        m_pRenderer.Draw(pMesh^,
                         pNextMesh^,
                         matrix,
                         interpolationFactor,
                         textures,
                         m_pShader);

        // notify user that collisions may be detected
        if (Assigned(m_fOnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
            m_fOnDetectCollisions(Self, matrix, pAABBTree);

        Exit;
    end;

    // do interpolate frames?
    if (not Assigned(pNextMesh) or (interpolationFactor <= 0.0)) then
    begin
        // draw mesh
        m_pRenderer.Draw(pMesh^, matrix, textures);

        // notify user that collisions may be detected
        if (Assigned(m_fOnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
            m_fOnDetectCollisions(Self, matrix, pAABBTree);

        Exit;
    end
    else
    if (interpolationFactor >= 1.0) then
    begin
        // draw mesh
        m_pRenderer.Draw(pNextMesh^, matrix, textures);

        // notify user that collisions may be detected
        if (Assigned(m_fOnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
            m_fOnDetectCollisions(Self, matrix, pNextAABBTree);

        Exit;
    end;

    // get next frame to draw
    TQRModelHelper.Interpolate(interpolationFactor, pMesh^, pNextMesh^, mesh);

    // draw mesh
    m_pRenderer.Draw(mesh, matrix, textures);

    // notify user that collisions may be detected
    if (Assigned(m_fOnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
        m_fOnDetectCollisions(Self, matrix, pAABBTree);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD3ModelGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLMD3ModelGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLMD3ModelGL)) then
    begin
        // reset values to default
        m_PackageName        := '';
        m_VertexName         := '';
        m_FragmentName       := '';
        m_TorsoGesture       := EQR_AT_MD3_Stand;
        m_LegsGesture        := EQR_AL_MD3_Idle;
        m_ModelOptions       := [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];
        m_FramedModelOptions := [];

        m_pPackage.Clear;
        m_pVertexShader.Clear;
        m_pFragmentShader.Clear;

        m_pModel.Assign(nil);

        SetTorsoGestureToModel;
        SetLegsGestureToModel;
        Exit;
    end;

    // copy content from source
    pSrc                 := pSource as TQRVCLMD3ModelGL;
    m_PackageName        := pSrc.m_PackageName;
    m_VertexName         := pSrc.m_VertexName;
    m_FragmentName       := pSrc.m_FragmentName;
    m_TorsoGesture       := pSrc.m_TorsoGesture;
    m_LegsGesture        := pSrc.m_LegsGesture;
    m_ModelOptions       := pSrc.m_ModelOptions;
    m_FramedModelOptions := pSrc.m_FramedModelOptions;

    m_pPackage.CopyFrom(pSrc.m_pPackage, pSrc.m_pPackage.Size);
    m_pVertexShader.CopyFrom(pSrc.m_pVertexShader, pSrc.m_pVertexShader.Size);
    m_pFragmentShader.CopyFrom(pSrc.m_pFragmentShader, pSrc.m_pFragmentShader.Size);

    m_pModel.Assign(pSrc.m_pModel);

    SetTorsoGestureToModel;
    SetLegsGestureToModel;
end;
//--------------------------------------------------------------------------------------------------

end.
