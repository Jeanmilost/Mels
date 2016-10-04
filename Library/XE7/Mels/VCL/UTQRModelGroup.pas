{**************************************************************************************************
 * ==> UTQRModelGroup ----------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module contains the classes used to load and link all model files together. *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

 unit UTQRModelGroup;

interface

uses System.Classes,
     System.SysUtils,
     System.Generics.Collections,
     System.Math,
     UTQRDesignPatterns,
     UTQRFiles,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRHelpers,
     UTQRModel,
     UTQRThreading,
     UTQRVCLHelpers,
     Vcl.Graphics,
     Vcl.Imaging.JPEG,
     Vcl.Imaging.GIFImg,
     Vcl.Imaging.PNGImage,
     Winapi.Windows;

type
    // TODO clear model parser when fully cached, also clear in-memory normals table

    {**
    * Messages a property can use to notify his owner
    *}
    EQRModelMessages =
    (
        EQR_MM_Loaded
    );

    {**
    * Model matrix combination type enumeration
    *}
    EQRModelMatrixCombinationType =
    (
        EQR_CT_Scale_Rotate_Translate,
        EQR_CT_Scale_Translate_Rotate,
        EQR_CT_Rotate_Translate_Scale,
        EQR_CT_Rotate_Scale_Translate,
        EQR_CT_Translate_Rotate_Scale,
        EQR_CT_Translate_Scale_Rotate
    );

    {**
    * Model option enumeration
    *}
    EQRModelOptions =
    (
        EQR_MO_Create_Cache,            // create cache in a thread while model is loaded
        EQR_MO_No_Collision,            // don't create data related to collisions, as e.g. AABB trees
        EQR_MO_Dynamic_Frames,          // frames are calculated dynamically while rendering
        EQR_MO_Dynamic_Frames_No_Cache, // dynamically calculated frames will not be cached
        EQR_MO_Without_Normals,         // do not generate normals while meshes are created
        EQR_MO_Without_Textures,        // do not generate textures while meshes are created
        EQR_MO_Without_Colors           // do not generate colors while meshes are created
    );

    {**
    * Model options set
    *}
    TQRModelOptions = set of EQRModelOptions;

    {**
    * Framed model option enumeration
    *}
    EQRFramedModelOptions =
    (
        EQR_FO_Start_Anim_When_Gesture_Is_Ready, // start anim when all frames belonging to selected gesture are loaded in cache
        EQR_FO_Show_Default_Frame,               // show default frame while cache is built
        EQR_FO_Interpolate                       // apply mesh interpolation internally
    );

    {**
    * Framed model options set
    *}
    TQRFramedModelOptions = set of EQRFramedModelOptions;

    TQRModelGroup = class;

    {**
    * Called when texture should be loaded
    *@param pGroup - group at which model belongs
    *@param pModel - model for which texture is required
    *@param pBitmap - whenever possible, the bitmap containing the texture, nil if not available
    *@param[in, out] pTexture - texture info, contains loaded index when function ends
    *@param [out] loadNext - if true, event will be called again with a new item to load next texture
    *@return true on success, otherwise false
    *}
    TQRLoadMeshTextureEvent = function(const pGroup: TQRModelGroup;
                                       const pModel: TQRModel;
                                            pBitmap: Vcl.Graphics.TBitmap;
                                           pTexture: TQRTexture;
                                       out loadNext: Boolean): Boolean of object;

    {**
    * Called after model was completely loaded
    *@param pGroup - group that finished to load the model
    *}
    TQRAfterLoadModelEvent = procedure(const pGroup: TQRModelGroup) of object;

    // model job status class prototype
    TQRModelJobStatus = class;

    {**
    * Model group helper, contains some common function used by models
    *}
    TQRModelGroupHelper = class
        public
            {**
            * Loads a texture from a stream
            *@param pStream - stream containing texture to load
            *@param extension - texture image extension
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            class function LoadTexture(const pStream: TStream;
                                     const extension: UnicodeString;
                                             pBitmap: Vcl.Graphics.TBitmap): Boolean; static;

            {**
            * Makes a texture power of 2
            *@param pSrcBitmap - source bitmap to transform
            *@param pDstBitmap - transformed destination bitmap
            *@return true on success, otherwise false
            *}
            class function MakeTexturePowerOf2(const pSrcBitmap: Vcl.Graphics.TBitmap;
                                                     pDstBitmap: Vcl.Graphics.TBitmap): Boolean; static;
    end;

    {**
    * Model group, contains all items and functions needed to manage a complete model
    *}
    TQRModelGroup = class
        protected
            m_pJobStatus:             TQRModelJobStatus;
            m_Scaling:                TQRVector3D;
            m_Translation:            TQRVector3D;
            m_RotationX:              TQRRotation;
            m_RotationY:              TQRRotation;
            m_RotationZ:              TQRRotation;
            m_SwapYZ:                 Boolean;
            m_CombinationType:        EQRModelMatrixCombinationType;
            m_pInitialMatrix:         PQRMatrix4x4;
            m_fOnLoadTexture:         TQRLoadMeshTextureEvent;
            m_fOnAfterLoadModelEvent: TQRAfterLoadModelEvent;

            {**
            * Gets scaling
            *@return scaling
            *}
            function GetScaling(): PQRVector3D; virtual;

            {**
            * Sets scaling
            *@param scaling - scaling
            *}
            procedure SetScaling(const pScaling: PQRVector3D); virtual;

            {**
            * Gets translation
            *@return Translation
            *}
            function GetTranslation(): PQRVector3D; virtual;

            {**
            * Sets translation
            *@param Translation - Translation
            *}
            procedure SetTranslation(const pTranslation: PQRVector3D); virtual;

            {**
            * Gets rotation on X axis
            *@return rotation angle on X axis in radians
            *}
            function GetRotationX(): Single; virtual;

            {**
            * Sets rotation on X axis
            *@param angle - rotation angle in radians
            *}
            procedure SetRotationX(const angle: Single); virtual;

            {**
            * Gets rotation on Y axis
            *@return rotation angle on Y axis in radians
            *}
            function GetRotationY(): Single; virtual;

            {**
            * Sets rotation on Y axis
            *@param angle - rotation angle in radians
            *}
            procedure SetRotationY(const angle: Single); virtual;

            {**
            * Gets rotation on Z axis
            *@return rotation angle on Z axis in radians
            *}
            function GetRotationZ(): Single; virtual;

            {**
            * Sets rotation on Z axis
            *@param angle - rotation angle in radians
            *}
            procedure SetRotationZ(const angle: Single); virtual;

            {**
            * Gets combination type
            *@return combination type
            *}
            function GetCombinationType(): EQRModelMatrixCombinationType; virtual;

            {**
            * Sets file name
            *@param value - combination type to set
            *}
            procedure SetCombinationType(value: EQRModelMatrixCombinationType); virtual;

            {**
            * Gets initial model matrix
            *@return matrix
            *}
            function GetInitialMatrix(): PQRMatrix4x4;

            {**
            * Sets initial model matrix
            *@param matrix - matrix
            *}
            procedure SetInitialMatrix(const pMatrix: PQRMatrix4x4);

            {**
            * Gets model matrix
            *@return matrix
            *}
            function GetMatrix(): TQRMatrix4x4; virtual;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Clears group
            *}
            procedure Clear(); virtual; abstract;

            {**
            * Checks if group is empty
            *@return true if model is empty, otherwise false
            *}
            function IsEmpty(): Boolean; virtual; abstract;

            {**
            * Queries the job status
            *@return job status
            *}
            function QueryJobStatus(): TQRModelJobStatus; virtual; abstract;

            {**
            * Draws group
            *@param elapsedTime - elapsed time since last draw
            *}
            procedure Draw(const elapsedTime: Double); virtual; abstract;

            {**
            * Called when subject send a notification to the observer
            *@param message - notification message
            *}
            procedure OnNotified(message: TQRMessage); virtual;

            { Properties }
            property Scaling:               PQRVector3D                   read GetScaling               write SetScaling;
            property Translation:           PQRVector3D                   read GetTranslation           write SetTranslation;
            property RotationX:             Single                        read GetRotationX             write SetRotationX;
            property RotationY:             Single                        read GetRotationY             write SetRotationY;
            property RotationZ:             Single                        read GetRotationZ             write SetRotationZ;
            property SwapYZ:                Boolean                       read m_SwapYZ                 write m_SwapYZ;
            property MatrixCombinationType: EQRModelMatrixCombinationType read GetCombinationType       write SetCombinationType default EQR_CT_Scale_Rotate_Translate;
            property InitialMatrix:         PQRMatrix4x4                  read GetInitialMatrix         write SetInitialMatrix   default nil;
            property OnLoadMeshTexture:     TQRLoadMeshTextureEvent       read m_fOnLoadTexture         write m_fOnLoadTexture;
            property OnAfterLoadModelEvent: TQRAfterLoadModelEvent        read m_fOnAfterLoadModelEvent write m_fOnAfterLoadModelEvent;
    end;

    {**
    * Called when static model item should be drawn
    *@param pGroup - group at which model belongs
    *@param pModel - model to draw
    *@param textures - textures belonging to model, in the order where they should be combined
    *@param matrix - model matrix
    *@param pMesh - mesh to draw, can be nil (depends on selected options)
    *@param pAABBTree - aligned-axis bounding box tree matching with mesh, can be nil (depends on selected options)
    *}
    TQRDrawStaticModelItemEvent = procedure (const pGroup: TQRModelGroup;
                                             const pModel: TQRModel;
                                           const textures: TQRTextures;
                                             const matrix: TQRMatrix4x4;
                                              const pMesh: PQRMesh;
                                          const pAABBTree: TQRAABBTree) of object;

    {**
    * Called when static model item should be drawn
    *@param pGroup - group at which model belongs
    *@param pModel - model to draw
    *@param textures - textures belonging to model, in the order where they should be combined
    *@param matrix - model matrix
    *}
    TQRDrawCustomStaticModelItemEvent = procedure (const pGroup: TQRModelGroup;
                                                         pModel: TQRModel;
                                                 const textures: TQRTextures;
                                                   const matrix: TQRMatrix4x4) of object;

    {**
    * Static model group, contains all items and functions needed to manage a complete static model
    *}
    TQRStaticModelGroup = class(TQRModelGroup)
        protected
            m_fOnDrawItem:       TQRDrawStaticModelItemEvent;
            m_fOnCustomDrawItem: TQRDrawCustomStaticModelItemEvent;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            { Properties }
            property OnDrawItem:       TQRDrawStaticModelItemEvent       read m_fOnDrawItem       write m_fOnDrawItem;
            property OnCustomDrawItem: TQRDrawCustomStaticModelItemEvent read m_fOnCustomDrawItem write m_fOnCustomDrawItem;
    end;

    {**
    * Model animation item, contains important animation info, as e.g. start frame, frame count,
    * FPS, ...
    *}
    TQRModelAnimCfgItem = record
        m_Gesture:         NativeInt;
        m_StartFrame:      NativeUInt;
        m_FrameCount:      NativeUInt;
        m_LoopingFrames:   NativeUInt;
        m_FramesPerSecond: NativeUInt;
    end;

    PQRModelAnimCfgItem  = ^TQRModelAnimCfgItem;
    TQRModelAnimCfgItems = array of TQRModelAnimCfgItem;

    {**
    * Framed model Animation data structure
    *}
    TQRFramedModelAnimation = class
        protected
            m_FrameIndex:              NativeUInt;
            m_InterpolationFrameIndex: NativeUInt;
            m_InterpolationFactor:     Double;
            m_Loop:                    Boolean;

        public
            {**
            * Constructor
            *@param frameIndex - frame index to render
            *@param interpolationFrameIndex - frame index to use for interpolation
            *@param interpolationFactor - interpolation factor in percent (between 0.0f and 1.0f)
            *@param loop - if true, animation should loop at end
            *}
            constructor Create(frameIndex, interpolationFrameIndex: NativeUInt;
                                               interpolationFactor: Double;
                                                              loop: Boolean); overload; virtual;

            { Construction/Destruction }
            constructor Create();  overload; virtual;
            destructor  Destroy(); override;

            { Properties }
            property FrameIndex:              NativeUInt read m_FrameIndex              write m_FrameIndex;
            property InterpolationFrameIndex: NativeUInt read m_InterpolationFrameIndex write m_InterpolationFrameIndex;
            property InterpolationFactor:     Double     read m_InterpolationFactor     write m_InterpolationFactor;
            property Loop:                    Boolean    read m_Loop                    write m_Loop                    default True;
    end;

    {**
    * Framed model animation configuration file parser
    *}
    TQRFramedModelAnimCfgFile = class(TQRScript)
        protected
            m_Items:       TQRModelAnimCfgItems;
            m_Column:      NativeUInt;
            m_LongComment: Boolean;

            {**
            * Called when script line should be parsed
            *@param line - line to parse
            *@param linbeNb - line number
            *@return ture on success, otherwise false
            *}
            function OnParseLine(const line: UnicodeString; lineNb: NativeUInt): Boolean; override;

            {**
            * Parses a word found in script line
            *@param word - word to parse
            *@param lineNb - current parsing line number
            *@return true on success, otherwise false
            *}
            function ParseWord(const word: UnicodeString;
                                   lineNb: NativeUInt): Boolean; virtual; abstract;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Clears script
            *}
            procedure Clear(); override;

            {**
            * Gets animation item at index
            *@return animation item at index, nil if not found or on error
            *}
            function GetItem(index: NativeInt): PQRModelAnimCfgItem; virtual;

            {**
            * Gets animation item count
            *@return animation item count
            *}
            function GetItemCount(): NativeInt; virtual;

            { Properties }
            property Items[index: NativeInt]: PQRModelAnimCfgItem read GetItem;
            property ItemCount:               NativeInt           read GetItemCount;
    end;

    {**
    * Called when framed model item should be drawn
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
    *}
    TQRDrawFramedModelItemEvent = procedure (const pGroup: TQRModelGroup;
                                             const pModel: TQRModel;
                                           const textures: TQRTextures;
                                             const matrix: TQRMatrix4x4;
                                         index, nextIndex: NativeInt;
                                const interpolationFactor: Double;
                                   const pMesh, pNextMesh: PQRMesh;
                           const pAABBTree, pNextAABBTree: TQRAABBTree) of object;

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
    TQRDrawCustomFramedModelItemEvent = procedure (const pGroup: TQRModelGroup;
                                                         pModel: TQRModel;
                                                 const textures: TQRTextures;
                                                   const matrix: TQRMatrix4x4;
                                               index, nextIndex: NativeInt;
                                      const interpolationFactor: Double) of object;

    {**
    * Called when a framed model animation ends
    *@param pGroup - group at which model belongs
    *@param pModel - model or sub-model for which animation ended
    *@param gesture - animation gesture that ended
    *}
    TQRFramedModelAnimEndEvent = procedure (const pGroup: TQRModelGroup;
                                            const pModel: TQRModel;
                                                 gesture: NativeInt) of object;

    {**
    * Framed model group, contains all items and functions needed to manage a complete framed model
    *}
    TQRFramedModelGroup = class(TQRModelGroup)
        protected
            m_Paused:            Boolean;
            m_ForceLoop:         Boolean;
            m_fOnDrawItem:       TQRDrawFramedModelItemEvent;
            m_fOnCustomDrawItem: TQRDrawCustomFramedModelItemEvent;
            m_fOnAnimEnd:        TQRFramedModelAnimEndEvent;

            {**
            * Gets animation
            *@param pModel - model or sub-model on which animation is applied
            *@param gesture - current running gesture
            *@param elapsedTime - elapsed time since last frame was drawn
            *@param fps - number of frames per seconds
            *@param startIndex - animation start index
            *@param endIndex - animation end index
            *@param loop - if true, animation should loop at end
            *@param[in, out] frameIndex - frame index, next frame index to draw when function ends
            *@param[in, out] interpolationFactor - interpolation factor between 2 frames
            *@param[in, out] endNotified - if true, animation end was already notified
            *@return animation
            *}
            function GetAnimation(const pModel: TQRModel;
                                       gesture: NativeInt;
                             const elapsedTime: Double;
                     fps, startIndex, endIndex: NativeUInt;
                                          loop: Boolean;
                                var frameIndex: NativeUInt;
                       var interpolationFactor: Double;
                               var endNotified: Boolean): TQRFramedModelAnimation; virtual;

            {**
            * Checks and returns an index that is always within the range delimited by start and end
            *@param index - index to check
            *@param startIndex - range start
            *@param endIndex - range end
            *@return an index that is always within the range delimited by start and end
            *@throw Exception if end index is smaller than start index
            *}
            function ValidateIndex(index, startIndex, endIndex: NativeUInt): NativeUInt; virtual;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            { Properties }
            property Pause:            Boolean                           read m_Paused            write m_Paused;
            property ForceLoop:        Boolean                           read m_ForceLoop         write m_ForceLoop   default True;
            property OnDrawItem:       TQRDrawFramedModelItemEvent       read m_fOnDrawItem       write m_fOnDrawItem;
            property OnCustomDrawItem: TQRDrawCustomFramedModelItemEvent read m_fOnCustomDrawItem write m_fOnCustomDrawItem;
            property OnAnimEnd:        TQRFramedModelAnimEndEvent        read m_fOnAnimEnd        write m_fOnAnimEnd;
    end;

    {**
    * Model job status, allows to query the job status
    *}
    TQRModelJobStatus = class
        protected
            m_JobStatus: EQRThreadJobStatus;
            m_Progress:  NativeUInt;

            {**
            * Sets progress
            *@param value - progress value (between 0 and 100)
            *}
            procedure SetProgress(value: NativeUInt); virtual;

        public
            { Construction/Destruction }
            constructor Create();  reintroduce;
            destructor  Destroy(); reintroduce;

            { Properties }
            property Status:   EQRThreadJobStatus read m_JobStatus write m_JobStatus;
            property Progress: NativeUInt         read m_Progress  write SetProgress;
    end;

    {**
    * Model job, allows to execute a heavy task, as e.g. load the model, in a separate thread
    *}
    TQRModelJob = class(TQRVCLThreadWorkerJob)
        protected
            m_pGroup:                 TQRModelGroup;
            m_pCache:                 TQRModelCache;
            m_ModelOptions:           TQRModelOptions;
            m_Progress:               Single;
            m_IsLoaded:               Boolean;
            m_TextureExt:             array [0..6] of UnicodeString;
            m_fOnAfterLoadModelEvent: TQRAfterLoadModelEvent;

            {**
            * Gets mesh
            *@return mesh, nil if not found or on error
            *}
            function GetMesh(i: NativeUInt): PQRMesh; virtual;

            {**
            * Sets mesh
            *@param pMesh - mesh
            *@note Be careful, the internal cache will take the mesh ownership, so don't try to
            *      delete it externally
            *}
            procedure SetMesh(i: NativeUInt; pMesh: PQRMesh); virtual;

            {**
            * Gets aligned-axis bounding box tree
            *@return tree, nil if not found or on error
            *}
            function GetTree(i: NativeUInt): TQRAABBTree; virtual;

            {**
            * Sets aligned-axis bounding box tree
            *@param pTree - tree
            *@note Be careful, the internal cache will take the tree ownership, so don't try to
            *      delete it externally
            *}
            procedure SetTree(i: NativeUInt; pTree: TQRAABBTree); virtual;

            {**
            * Gets job progress
            *@return job progress in percent (between 0 and 100)
            *}
            function GetProgress(): Single; virtual;

            {**
            * Sets job progress
            *@param value - job progress value (between 0 and 100)
            *}
            procedure SetProgress(value: Single); virtual;

            {**
            * Gets model options
            *@return model options
            *}
            function GetModelOptions(): TQRModelOptions; virtual;

            {**
            * Sets model options
            *@param options - model options
            *}
            procedure SetModelOptions(options: TQRModelOptions); virtual;

            {**
            * Called before a texture is loaded
            *@param pTexture - texture to load
            *@param custom - if true, texture is a custom user, otherwise a texture belonging to model
            *}
            procedure BeforeLoadTexture(pTexture: TQRTexture;
                                          custom: Boolean); virtual; abstract;

            {**
            * Called when a known texture should be loaded
            *@param pTexture - texture to load
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; virtual; abstract;

            {**
            * Called after model is loaded
            *}
            procedure OnAfterLoadModel(); virtual;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param modelOptions - model options
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup; modelOptions: TQRModelOptions); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;

            {**
            * Gets group that started the job
            *@return group that started the job
            *}
            function GetGroup(): TQRModelGroup; virtual;

            { Properties }
            property Mesh[i: NativeUInt]:     PQRMesh                read GetMesh                  write SetMesh;
            property AABBTree[i: NativeUInt]: TQRAABBTree            read GetTree                  write SetTree;
            property Progress:                Single                 read GetProgress              write SetProgress;
            property ModelOptions:            TQRModelOptions        read GetModelOptions          write SetModelOptions;
            property OnAfterLoadModelEvent:   TQRAfterLoadModelEvent read m_fOnAfterLoadModelEvent write m_fOnAfterLoadModelEvent;
    end;

    {**
    * Model job list
    *}
    TQRModelJobs = TList<TQRModelJob>;

    {**
    * Model worker, it's a specialized class whose role is to carry out model jobs, as e.g. load a
    * model in memory and prepare his cache
    *}
    TQRModelWorker = class sealed (TObject)
        private
            class var m_pInstance: TQRModelWorker;
                      m_pWorker:   TQRVCLThreadWorker;
                      m_pGarbage:  TList<TQRThreadJob>;

            {**
            * Called when a job is done
            *@param pJob - done job
            *}
            procedure OnThreadJobDone(pJob: TQRThreadJob);

        public
            { Construction/Destruction }
            constructor Create();
            destructor  Destroy(); override;

            {**
            * Gets model cache instance, creates one if still not created
            *@return model cache instance
            *}
            class function GetInstance(): TQRModelWorker; static;

            {**
            * Deletes model cache instance
            *@note This function is automatically called when unit is released
            *}
            class procedure DeleteInstance(); static;

            {**
            * Starts the job
            *@param pJob - job to execute
            *@return true on success, otherwise false
            *}
            function StartJob(pJob: TQRModelJob): Boolean;

            {**
            * Cancels the job execution
            *@param pJob - job to cancel
            *}
            procedure CancelJob(pJob: TQRModelJob);
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRModelGroupHelper
//--------------------------------------------------------------------------------------------------
class function TQRModelGroupHelper.LoadTexture(const pStream: TStream;
                                             const extension: UnicodeString;
                                                     pBitmap: Vcl.Graphics.TBitmap): Boolean;
var
    ext:   UnicodeString;
    pJPEG: TJpegImage;
    pPNG:  TPngImage;
    pGIF:  TGifImage;
begin
    // no stream to load from?
    if (not Assigned(pStream)) then
    begin
        Result := False;
        Exit;
    end;

    // no texture bitmap to load to?
    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // convert extension to lower case
    ext := LowerCase(extension);

    // search for texture image type to load
    if (ext = '.bmp') then
    begin
        // bitmap image
        pBitmap.LoadFromStream(pStream);

        // is image empty?
        if ((pBitmap.Width = 0) or (pBitmap.Height = 0)) then
        begin
            Result := False;
            Exit;
        end;
    end
    else
    if ((ext = '.jpg') or (extension = '.jpeg')) then
    begin
        // jpeg image
        pJPEG := TJpegImage.Create;

        try
            pJPEG.LoadFromStream(pStream);
            pBitmap.Assign(pJPEG);

            // is image empty?
            if ((pBitmap.Width = 0) or (pBitmap.Height = 0)) then
            begin
                Result := False;
                Exit;
            end;
        finally
            pJPEG.Free;
        end;
    end
    else
    if (ext = '.png') then
    begin
        // png image
        pPNG := TPngImage.Create;

        try
            pPNG.LoadFromStream(pStream);
            pBitmap.Assign(pPNG);

            // is image empty?
            if ((pBitmap.Width = 0) or (pBitmap.Height = 0)) then
            begin
                Result := False;
                Exit;
            end;
        finally
            pPNG.Free;
        end;
    end
    else
    if (ext = '.gif') then
    begin
        // gif image
        pGIF := TGifImage.Create;

        try
            pGIF.LoadFromStream(pStream);
            pBitmap.Assign(pGIF);

            // is image empty?
            if ((pBitmap.Width = 0) or (pBitmap.Height = 0)) then
            begin
                Result := False;
                Exit;
            end;
        finally
            pGIF.Free;
        end;
    end
    else
    if (ext = '.tga') then
    begin
        // targa (.tga) image
        if (not TQRVCLPictureHelper.LoadTGA(pStream, pStream.Size, False, pBitmap)) then
        begin
            Result := False;
            Exit;
        end;

        // is image empty?
        if ((pBitmap.Width = 0) or (pBitmap.Height = 0)) then
        begin
            Result := False;
            Exit;
        end;
    end
    else
    if (ext = '.pcx') then
    begin
        // personal computer exchange (.pcx) image
        if (not TQRVCLPictureHelper.LoadPCX(pStream, pStream.Size, pBitmap)) then
        begin
            Result := False;
            Exit;
        end;

        // is image empty?
        if ((pBitmap.Width = 0) or (pBitmap.Height = 0)) then
        begin
            Result := False;
            Exit;
        end;
    end
    else
    begin
        Result := False;
        Exit;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
class function TQRModelGroupHelper.MakeTexturePowerOf2(const pSrcBitmap: Vcl.Graphics.TBitmap;
                                                             pDstBitmap: Vcl.Graphics.TBitmap): Boolean;
var
    pPoweredBmp: Vcl.Graphics.TBitmap;
    prevMode:    Integer;
begin
    // no source bitmap?
    if (not Assigned(pSrcBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // no destination bitmap?
    if (not Assigned(pDstBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // is source bitmap empty?
    if ((pSrcBitmap.Width = 0) or (pSrcBitmap.Height = 0)) then
    begin
        Result := False;
        Exit;
    end;

    // is texture already a power of 2?
    if (TQRMathsHelper.IsPowerOfTwo(pSrcBitmap.Width) and
        TQRMathsHelper.IsPowerOfTwo(pSrcBitmap.Height))
    then
    begin
        // copy source to destination
        pDstBitmap.Assign(pSrcBitmap);
        Result := True;
        Exit;
    end;

    // create a bitmap that will contain the texture rounded to the nearest power of 2 values
    pPoweredBmp := Vcl.Graphics.TBitmap.Create();

    try
        pPoweredBmp.PixelFormat := pSrcBitmap.PixelFormat;
        pPoweredBmp.AlphaFormat := pSrcBitmap.AlphaFormat;
        pPoweredBmp.SetSize(TQRMathsHelper.GetClosestPowerOf2(pSrcBitmap.Width),
                            TQRMathsHelper.GetClosestPowerOf2(pSrcBitmap.Height));

        // set stretch mode to half tones (thus resizing will be smooth)
        prevMode := SetStretchBltMode(pPoweredBmp.Canvas.Handle, HALFTONE);

        try
            // make texture size power of 2
            StretchBlt(pPoweredBmp.Canvas.Handle,
                       0,
                       0,
                       pPoweredBmp.Width,
                       pPoweredBmp.Height,
                       pSrcBitmap.Canvas.Handle,
                       0,
                       0,
                       pSrcBitmap.Width,
                       pSrcBitmap.Height,
                       SRCCOPY);
        finally
            // restore previous stretch blit mode
            SetStretchBltMode(pPoweredBmp.Canvas.Handle, prevMode);
        end;

        // replace texture py the powered one
        pDstBitmap.Assign(pPoweredBmp);
    finally
        // clear memory
        pPoweredBmp.Free;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRModelGroup
//--------------------------------------------------------------------------------------------------
constructor TQRModelGroup.Create();
begin
    inherited Create;

    m_pJobStatus             := TQRModelJobStatus.Create;
    m_Scaling                := TQRVector3D.Create(1.0, 1.0, 1.0);
    m_Translation            := TQRVector3D.Create(0.0, 0.0, 0.0);
    m_RotationX              := TQRRotation.Create(0.0, TQRVector3D.Create(1.0, 0.0, 0.0));
    m_RotationY              := TQRRotation.Create(0.0, TQRVector3D.Create(0.0, 1.0, 0.0));
    m_RotationZ              := TQRRotation.Create(0.0, TQRVector3D.Create(0.0, 0.0, 1.0));
    m_SwapYZ                 := False;
    m_CombinationType        := EQR_CT_Scale_Rotate_Translate;
    m_pInitialMatrix         := nil;
    m_fOnLoadTexture         := nil;
    m_fOnAfterLoadModelEvent := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRModelGroup.Destroy();
begin
    // delete initial matrix, if exists
    if (Assigned(m_pInitialMatrix)) then
        Dispose(m_pInitialMatrix);

    // delete rotations
    m_RotationX.Free;
    m_RotationY.Free;
    m_RotationZ.Free;

    // delete job status
    m_pJobStatus.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelGroup.GetScaling(): PQRVector3D;
begin
    Result := @m_Scaling;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelGroup.SetScaling(const pScaling: PQRVector3D);
begin
    m_Scaling := pScaling^;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelGroup.GetTranslation(): PQRVector3D;
begin
    Result := @m_Translation;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelGroup.SetTranslation(const pTranslation: PQRVector3D);
begin
    m_Translation := pTranslation^;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelGroup.GetRotationX(): Single;
begin
    Result := m_RotationX.Angle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelGroup.SetRotationX(const angle: Single);
begin
    m_RotationX.Angle := angle;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelGroup.GetRotationY(): Single;
begin
    Result := m_RotationY.Angle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelGroup.SetRotationY(const angle: Single);
begin
    m_RotationY.Angle := angle;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelGroup.GetRotationZ(): Single;
begin
    Result := m_RotationZ.Angle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelGroup.SetRotationZ(const angle: Single);
begin
    m_RotationZ.Angle := angle;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelGroup.GetCombinationType(): EQRModelMatrixCombinationType;
begin
    Result := m_CombinationType;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelGroup.SetCombinationType(value: EQRModelMatrixCombinationType);
begin
    m_CombinationType := value;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelGroup.GetInitialMatrix(): PQRMatrix4x4;
begin
    Result := m_pInitialMatrix;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelGroup.SetInitialMatrix(const pMatrix: PQRMatrix4x4);
begin
    // nothing changed?
    if (pMatrix = m_pInitialMatrix) then
        Exit;

    // clear memory, if needed
    if (Assigned(m_pInitialMatrix)) then
    begin
        Dispose(m_pInitialMatrix);
        m_pInitialMatrix := nil;
    end;

    // no new initial matrix to set?
    if (not Assigned(pMatrix)) then
        Exit;

    // create new initial matrix and copy content from value
    New(m_pInitialMatrix);
    m_pInitialMatrix.Assign(pMatrix^);
end;
//--------------------------------------------------------------------------------------------------
function TQRModelGroup.GetMatrix(): TQRMatrix4x4;
var
    scaleMatrix,
    rotateXMatrix,
    rotateYMatrix,
    rotateZMatrix,
    translateMatrix,
    modelMatrix:     TQRMatrix4x4;
begin
    // initialize matrix
    scaleMatrix     := TQRMatrix4x4.Identity();
    rotateXMatrix   := TQRMatrix4x4.Identity();
    rotateYMatrix   := TQRMatrix4x4.Identity();
    rotateZMatrix   := TQRMatrix4x4.Identity();
    translateMatrix := TQRMatrix4x4.Identity();

    if (Assigned(m_pInitialMatrix)) then
        modelMatrix := m_pInitialMatrix^
    else
        modelMatrix := TQRMatrix4x4.Identity();

    // do swap y and z axis?
    if (m_SwapYZ) then
    begin
        // build scaling, rotation and translation matrix, swap y and z axis
        scaleMatrix.Scale(m_Scaling);
        rotateXMatrix.Rotate(m_RotationX.Angle, m_RotationX.Axis^);
        rotateYMatrix.Rotate(m_RotationZ.Angle, m_RotationY.Axis^);
        rotateZMatrix.Rotate(m_RotationY.Angle, m_RotationZ.Axis^);
        translateMatrix.Translate(m_Translation);
    end
    else
    begin
        // build scaling, rotation and translation matrix
        scaleMatrix.Scale(m_Scaling);
        rotateXMatrix.Rotate(m_RotationX.Angle, m_RotationX.Axis^);
        rotateYMatrix.Rotate(m_RotationY.Angle, m_RotationY.Axis^);
        rotateZMatrix.Rotate(m_RotationZ.Angle, m_RotationZ.Axis^);
        translateMatrix.Translate(m_Translation);
    end;

    // build model matrix
    case m_CombinationType of
        EQR_CT_Scale_Rotate_Translate:
        begin
            modelMatrix := modelMatrix.Multiply(scaleMatrix);
            modelMatrix := modelMatrix.Multiply(rotateXMatrix);
            modelMatrix := modelMatrix.Multiply(rotateYMatrix);
            modelMatrix := modelMatrix.Multiply(rotateZMatrix);
            modelMatrix := modelMatrix.Multiply(translateMatrix);
        end;

        EQR_CT_Scale_Translate_Rotate:
        begin
            modelMatrix := modelMatrix.Multiply(scaleMatrix);
            modelMatrix := modelMatrix.Multiply(translateMatrix);
            modelMatrix := modelMatrix.Multiply(rotateXMatrix);
            modelMatrix := modelMatrix.Multiply(rotateYMatrix);
            modelMatrix := modelMatrix.Multiply(rotateZMatrix);
        end;

        EQR_CT_Rotate_Translate_Scale:
        begin
            modelMatrix := modelMatrix.Multiply(rotateXMatrix);
            modelMatrix := modelMatrix.Multiply(rotateYMatrix);
            modelMatrix := modelMatrix.Multiply(rotateZMatrix);
            modelMatrix := modelMatrix.Multiply(translateMatrix);
            modelMatrix := modelMatrix.Multiply(scaleMatrix);
        end;

        EQR_CT_Rotate_Scale_Translate:
        begin
            modelMatrix := modelMatrix.Multiply(rotateXMatrix);
            modelMatrix := modelMatrix.Multiply(rotateYMatrix);
            modelMatrix := modelMatrix.Multiply(rotateZMatrix);
            modelMatrix := modelMatrix.Multiply(scaleMatrix);
            modelMatrix := modelMatrix.Multiply(translateMatrix);
        end;

        EQR_CT_Translate_Rotate_Scale:
        begin
            modelMatrix := modelMatrix.Multiply(translateMatrix);
            modelMatrix := modelMatrix.Multiply(rotateXMatrix);
            modelMatrix := modelMatrix.Multiply(rotateYMatrix);
            modelMatrix := modelMatrix.Multiply(rotateZMatrix);
            modelMatrix := modelMatrix.Multiply(scaleMatrix);
        end;

        EQR_CT_Translate_Scale_Rotate:
        begin
            modelMatrix := modelMatrix.Multiply(translateMatrix);
            modelMatrix := modelMatrix.Multiply(scaleMatrix);
            modelMatrix := modelMatrix.Multiply(rotateXMatrix);
            modelMatrix := modelMatrix.Multiply(rotateYMatrix);
            modelMatrix := modelMatrix.Multiply(rotateZMatrix);
        end;
    else
        raise Exception.Create('Unknown model matrix combination type');
    end;

    Result := modelMatrix;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelGroup.OnNotified(message: TQRMessage);
begin
    case (EQRModelMessages(message.m_Type)) of
        EQR_MM_Loaded:
        begin
            if (Assigned(m_fOnAfterLoadModelEvent)) then
                m_fOnAfterLoadModelEvent(Self);
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRFramedModelAnimation
//--------------------------------------------------------------------------------------------------
constructor TQRFramedModelAnimation.Create(frameIndex, interpolationFrameIndex: NativeUInt;
                                                           interpolationFactor: Double;
                                                                          loop: Boolean);
begin
    inherited Create;

    m_FrameIndex              := frameIndex;
    m_InterpolationFrameIndex := interpolationFrameIndex;
    m_InterpolationFactor     := interpolationFactor;
    m_Loop                    := loop;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRFramedModelAnimation.Create();
begin
    inherited Create;

    m_FrameIndex              := 0;
    m_InterpolationFrameIndex := 0;
    m_InterpolationFactor     := 0.0;
    m_Loop                    := True;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRFramedModelAnimation.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRStaticModelGroup
//--------------------------------------------------------------------------------------------------
constructor TQRStaticModelGroup.Create();
begin
    inherited Create;

    m_fOnDrawItem       := nil;
    m_fOnCustomDrawItem := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRStaticModelGroup.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRFramedModelAnimCfgFile
//--------------------------------------------------------------------------------------------------
constructor TQRFramedModelAnimCfgFile.Create();
begin
    inherited Create;

    m_Column      := 0;
    m_LongComment := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRFramedModelAnimCfgFile.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRFramedModelAnimCfgFile.OnParseLine(const line: UnicodeString;
                                                   lineNb: NativeUInt): Boolean;
var
    data, word:    UnicodeString;
    commentPos, i: NativeInt;
begin
    m_Column := 0;

    // is compiling on XE2 or earlier?
    {$IF CompilerVersion < 24}
        // no line to parse?
        if (Length(line) = 0) then
    {$ELSE}
        // no line to parse?
        if (line.IsEmpty) then
    {$IFEND}
        begin
            Result := True;
            Exit;
        end;

    // is compiling on XE2 or earlier?
    {$IF CompilerVersion < 24}
        // search for comment marker
        commentPos := System.Pos('//', line) - 1;

        // if not found, set to -1 (it's the position that IndexOf() returns in this case)
        if (commentPos = 0) then
            commentPos := -1;
    {$ELSE}
        // search for comment marker
        commentPos := line.IndexOf('//', 1);
    {$IFEND}

    // found it?
    if (commentPos = 0) then
        // no, parse entire line
        data := line
    else
        // is compiling on XE2 or earlier?
        {$IF CompilerVersion < 24}
            // yes, parse only the uncommented line part
            data := Trim(System.Copy(line, 1, commentPos - 1));
        {$ELSE}
            // yes, parse only the uncommented line part
            data := line.Substring(0, commentPos - 1).Trim;
        {$IFEND}

    // is compiling on XE2 or earlier?
    {$IF CompilerVersion < 24}
        // nothing to parse?
        if (Length(data) = 0) then
    {$ELSE}
        // nothing to parse?
        if (data.IsEmpty) then
    {$IFEND}
        begin
            Result := True;
            Exit;
        end;

    // don't forget, the UnicodeString index system is 1 based
    i    := 1;
    word := '';

    // is compiling on XE2 or earlier?
    {$IF CompilerVersion < 24}
        // iterate through line chars
        while (i <= Length(data)) do
    {$ELSE}
        // iterate through line chars
        while (i <= data.Length) do
    {$IFEND}
        begin
            // search for char
            case (data[i]) of
                '/',
                '*':
                begin
                    // is compiling on XE2 or earlier?
                    {$IF CompilerVersion < 24}
                        // found a long comment (i.e. comment between /* and */) start or end mark?
                        if ((i + 1) <= Length(data)) then
                    {$ELSE}
                        // found a long comment (i.e. comment between /* and */) start or end mark?
                        if ((i + 1) <= data.Length) then
                    {$IFEND}
                            if ((data[i] = '/') and (data[i + 1] = '*')) then
                            begin
                                m_LongComment := True;
                                Inc(i);
                            end
                            else
                            if ((data[i] = '*') and (data[i + 1] = '/')) then
                            begin
                                m_LongComment := False;
                                Inc(i);
                            end;
                end;

                ' ',
                #09:
                begin
                    // skip all chars inside a long comment
                    if (m_LongComment) then
                    begin
                        Inc(i);
                        continue;
                    end;

                    // is compiling on XE2 or earlier?
                    {$IF CompilerVersion < 24}
                        // found word to parse?
                        if (Length(word) > 0) then
                    {$ELSE}
                        // found word to parse?
                        if (not word.IsEmpty) then
                    {$IFEND}
                        begin
                            // parse it
                            if (not ParseWord(word, lineNb)) then
                            begin
                                Result := False;
                                Exit;
                            end;

                            // clear parsed word to read next
                            word := '';
                        end;

                    // is compiling on XE2 or earlier?
                    {$IF CompilerVersion < 24}
                        // skip all remaining spaces
                        while (((i + 1) <= Length(data)) and
                               ((data[i + 1] = ' ') or (data[i + 1] = '\t')))
                        do
                    {$ELSE}
                        // skip all remaining spaces
                        while (((i + 1) <= data.Length) and
                               ((data[i + 1] = ' ') or (data[i + 1] = '\t')))
                        do
                    {$IFEND}
                            Inc(i);
                end
            else
                // skip all chars inside a long comment
                if (m_LongComment) then
                begin
                    Inc(i);
                    continue;
                end;

                // add char to word
                word := word + data[i];
            end;

            Inc(i);
        end;

    // skip all chars inside a long comment
    if (m_LongComment) then
    begin
        Result := True;
        Exit;
    end;

    // is compiling on XE2 or earlier?
    {$IF CompilerVersion < 24}
        // last word to parse?
        if (Length(word) > 0) then
    {$ELSE}
        // last word to parse?
        if (not word.IsEmpty) then
    {$IFEND}
        begin
            // parse it
            Result := ParseWord(word, lineNb);
            Exit;
        end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRFramedModelAnimCfgFile.Clear();
begin
    // clear items
    SetLength(m_Items, 0);

    // reset values
    m_Column      := 0;
    m_LongComment := False;
end;
//--------------------------------------------------------------------------------------------------
function TQRFramedModelAnimCfgFile.GetItem(index: NativeInt): PQRModelAnimCfgItem;
begin
    if (index >= Length(m_Items)) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Items[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRFramedModelAnimCfgFile.GetItemCount(): NativeInt;
begin
    Result := Length(m_Items);
end;
//--------------------------------------------------------------------------------------------------
// TQRFramedModelGroup
//--------------------------------------------------------------------------------------------------
constructor TQRFramedModelGroup.Create();
begin
    inherited Create;

    m_Paused            := False;
    m_ForceLoop         := True;
    m_fOnDrawItem       := nil;
    m_fOnCustomDrawItem := nil;
    m_fOnAnimEnd        := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRFramedModelGroup.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRFramedModelGroup.GetAnimation(const pModel: TQRModel;
                                               gesture: NativeInt;
                                     const elapsedTime: Double;
                             fps, startIndex, endIndex: NativeUInt;
                                                  loop: Boolean;
                                        var frameIndex: NativeUInt;
                               var interpolationFactor: Double;
                                       var endNotified: Boolean): TQRFramedModelAnimation;
var
    timeInterval:   Double;
    frameCount:     NativeUInt;
    nextFrameIndex: NativeUInt;
begin
    // no animation info?
    if ((fps = 0) or (startIndex = endIndex)) then
    begin
        Result := TQRFramedModelAnimation.Create(startIndex, startIndex, 0.0, False);
        Exit;
    end;

    // is animation paused?
    if (m_Paused) then
    begin
        Result := TQRFramedModelAnimation.Create(frameIndex, frameIndex, interpolationFactor, loop);
        Exit;
    end;

    // calculate time interval between each frames
    timeInterval := (1000.0 / fps);

    // calculate how many frames must be incremented since the last rendering
    interpolationFactor := interpolationFactor + (elapsedTime / timeInterval);

    // should increment one frame or more?
    if (interpolationFactor >= 1.0) then
    begin
        // calculate number of frames to increment
        frameCount := Floor(interpolationFactor);

        // calculate interpolation factor (should always be between 0 and 1)
        interpolationFactor := interpolationFactor - frameCount;

        // move frame index to next frame to show
        Inc(frameIndex, frameCount);
    end;

    // not looping?
    if ((not m_ForceLoop) and (not loop)) then
    begin
        // last frame reached?
        if (frameIndex > endIndex) then
        begin
            // show last frame
            frameIndex          := endIndex;
            nextFrameIndex      := endIndex;
            interpolationFactor := 0.0;

            // animation end reached?
            if (Assigned(m_fOnAnimEnd) and (not endNotified)) then
            begin
                m_fOnAnimEnd(Self, pModel, gesture);
                endNotified := True;
            end;
        end
        else
        begin
            // show next frame
            nextFrameIndex := frameIndex + 1;

            if (nextFrameIndex > endIndex) then
                nextFrameIndex := endIndex;
        end;

        Result := TQRFramedModelAnimation.Create(frameIndex,
                                                 nextFrameIndex,
                                                 interpolationFactor,
                                                 loop);
        Exit;
    end;

    // calculate next indexes to render
    frameIndex     := ValidateIndex(frameIndex,     startIndex, endIndex);
    nextFrameIndex := ValidateIndex(frameIndex + 1, startIndex, endIndex);

    // return animation data
    Result := TQRFramedModelAnimation.Create(frameIndex,
                                             nextFrameIndex,
                                             interpolationFactor,
                                             loop);
end;
//--------------------------------------------------------------------------------------------------
function TQRFramedModelGroup.ValidateIndex(index, startIndex, endIndex: NativeUInt): NativeUInt;
var
    range: NativeUInt;
begin
    // is range invalid?
    if (endIndex < startIndex) then
        raise Exception.Create('Invalid range');

    // is index out of bounds?
    if ((index >= startIndex) and (index <= endIndex)) then
    begin
        Result := index;
        Exit;
    end;

    // calculate range
    range := (endIndex - startIndex) + 1;

    // calculate and return index within the range delimited by start and end
    Result := (startIndex + ((index - startIndex) mod range));
end;
//--------------------------------------------------------------------------------------------------
// TQRModelJobStatus
//--------------------------------------------------------------------------------------------------
constructor TQRModelJobStatus.Create();
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRModelJobStatus.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelJobStatus.SetProgress(value: NativeUInt);
begin
    // set new progress, limit value between 0 and 100
    if (value > 100) then
        m_Progress := 100
    else
        m_Progress := value;
end;
//--------------------------------------------------------------------------------------------------
// TQRModelJob
//--------------------------------------------------------------------------------------------------
constructor TQRModelJob.Create(pGroup: TQRModelGroup; modelOptions: TQRModelOptions);
begin
    inherited Create;

    // don't allow to create a job without group
    if (not Assigned(pGroup)) then
        raise Exception.Create('Model job cannot be created without group');

    m_pGroup                 := pGroup;
    m_pCache                 := TQRModelCache.Create;
    m_ModelOptions           := modelOptions;
    m_Progress               := 0.0;
    m_IsLoaded               := False;
    m_fOnAfterLoadModelEvent := nil;

    // set available texture formats
    m_TextureExt[0] := '.bmp';
    m_TextureExt[1] := '.jpg';
    m_TextureExt[2] := '.jpeg';
    m_TextureExt[3] := '.png';
    m_TextureExt[4] := '.gif';
    m_TextureExt[5] := '.tga';
    m_TextureExt[6] := '.pcx';
end;
//--------------------------------------------------------------------------------------------------
destructor TQRModelJob.Destroy();
begin
    // clear memory
    m_pLock.Lock;
    m_pCache.Free;
    m_pLock.Unlock;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelJob.GetMesh(i: NativeUInt): PQRMesh;
begin
    m_pLock.Lock;
    Result := m_pCache.Mesh[i];
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelJob.SetMesh(i: NativeUInt; pMesh: PQRMesh);
begin
    m_pLock.Lock;
    m_pCache.Mesh[i] := pMesh;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelJob.GetTree(i: NativeUInt): TQRAABBTree;
begin
    m_pLock.Lock;
    Result := m_pCache.AABBTree[i];
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelJob.SetTree(i: NativeUInt; pTree: TQRAABBTree);
begin
    m_pLock.Lock;
    m_pCache.AABBTree[i] := pTree;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelJob.GetGroup(): TQRModelGroup;
begin
    m_pLock.Lock;
    Result := m_pGroup;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelJob.GetProgress(): Single;
begin
    m_pLock.Lock;
    Result := m_Progress;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelJob.SetProgress(value: Single);
begin
    m_pLock.Lock;

    // set new progress, limit value between 0 and 100
    if (value > 100.0) then
        m_Progress := 100.0
    else
        m_Progress := value;

    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelJob.GetModelOptions(): TQRModelOptions;
begin
    m_pLock.Lock;
    Result := m_ModelOptions;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelJob.SetModelOptions(options: TQRModelOptions);
begin
    m_pLock.Lock;
    m_ModelOptions := options;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelJob.OnAfterLoadModel();
var
    msg: TQRMessage;
begin
    m_pLock.Lock;

    try
        if (not Assigned(m_pGroup)) then
            Exit;

        // build notification message
        msg.m_Type  := NativeUInt(EQR_MM_Loaded);
        msg.m_pInfo := nil;

        // notify group that model is loaded
        m_pGroup.OnNotified(msg);
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRModelWorker
//--------------------------------------------------------------------------------------------------
constructor TQRModelWorker.Create();
begin
    // singleton was already initialized?
    if (Assigned(m_pInstance)) then
        raise Exception.Create('Cannot create many instances of a singleton class');

    inherited Create;

    // create the garbage collector
    m_pGarbage := TList<TQRThreadJob>.Create;

    // create and configure threaded job worker
    m_pWorker        := TQRVCLThreadWorker.Create;
    m_pWorker.OnDone := OnThreadJobDone;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRModelWorker.Destroy();
var
    i: NativeInt;
begin
    // cancel all jobs and free the worker
    m_pWorker.Cancel;
    m_pWorker.Free;

    // clear eventual remaining jobs
    if (m_pGarbage.Count > 0) then
        for i := 0 to m_pGarbage.Count - 1 do
            m_pGarbage[i].Free;

    // clear garbage collector
    m_pGarbage.Free;

    inherited Destroy;

    m_pInstance := nil;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelWorker.OnThreadJobDone(pJob: TQRThreadJob);
var
    i: NativeInt;
begin
    // search for done job in garbage collector
    if (m_pGarbage.Count > 0) then
        for i := 0 to m_pGarbage.Count - 1 do
            // found it?
            if (pJob = m_pGarbage[i]) then
            begin
                // clear done job
                m_pGarbage[i].Free;
                m_pGarbage.Delete(i);
                break;
            end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRModelWorker.GetInstance(): TQRModelWorker;
begin
    // is singleton instance already initialized?
    if (Assigned(m_pInstance)) then
    begin
        // get it
        Result := m_pInstance;
        Exit;
    end;

    // create new singleton instance
    m_pInstance := TQRModelWorker.Create;
    Result      := m_pInstance;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRModelWorker.DeleteInstance();
begin
    m_pInstance.Free;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelWorker.StartJob(pJob: TQRModelJob): Boolean;
begin
    m_pWorker.AddJob(pJob);
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelWorker.CancelJob(pJob: TQRModelJob);
begin
    // no job to cancel?
    if (not Assigned(pJob)) then
        Exit;

    // delete job in worker
    m_pWorker.DeleteJob(pJob, True);

    // release job, postpone destruction if still processed in worker
    if (pJob <> m_pWorker.ProcessingJob) then
        pJob.Free
    else
        m_pGarbage.Add(pJob);
end;
//--------------------------------------------------------------------------------------------------

initialization
//--------------------------------------------------------------------------------------------------
// TQRModelWorker
//--------------------------------------------------------------------------------------------------
begin
    // initialize cache instance to default when application opens
    TQRModelWorker.m_pInstance := nil;
end;
//--------------------------------------------------------------------------------------------------

finalization
//--------------------------------------------------------------------------------------------------
// TQRModelWorker
//--------------------------------------------------------------------------------------------------
begin
    // free instance when application closes
    TQRModelWorker.DeleteInstance();
end;
//--------------------------------------------------------------------------------------------------

end.
