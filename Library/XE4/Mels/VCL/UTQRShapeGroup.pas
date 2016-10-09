{**************************************************************************************************
 * ==> UTQRShapeGroup ---------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module contains the classes used to load and link all shape files together. *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRShapeGroup;

interface

uses System.Classes,
     System.Math,
     System.SysUtils,
     UTQRCommon,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRShapes,
     UTQRModelGroup,
     UTQRThreading,
     UTQRLogging,
     Vcl.Graphics;

type
    {**
    * Shape helper
    *}
    TQRShapeHelper = class
        public
            {**
            * Adds vertex to a vertex buffer
            *@param pPosition - vertex possition in space 3D coordinates
            *@param pNormal - vertex normal
            *@param pTexCoord - vertex texture coordinates
            *@param pColor - vertex color
            *@param[in, out] index - vertex index in the buffer
            *@param[in, out] vertex - vertex info containing buffer in which vertex should be added
            *}
            class procedure AddVertex(const pPosition, pNormal: PQRVector3D;
                                               const pTexCoord: PQRVector2D;
                                                  const pColor: TQRColor;
                                                     var index: NativeUInt;
                                                    var vertex: TQRVertex); static;
    end;

    {**
    * Generic 3D shape job
    *}
    TQRShapeJob = class(TQRModelJob)
        protected
            m_pModel:         TQRShapeModel;
            m_Textures:       TQRTextures;
            m_pColor:         TQRColor;
            m_MaxTexture:     NativeUInt;
            m_TextureLoaded:  Boolean;
            m_IsCanceled:     Boolean;
            m_fOnLoadTexture: TQRLoadMeshTextureEvent;

            {**
            * Gets model
            *@return model
            *}
            function GetModel(): TQRShapeModel; virtual;

            {**
            * Gets texture
            *@param index - texture index to get
            *@return texture
            *}
            function GetTexture(index: NativeInt): TQRTexture; virtual;

            {**
            * Gets texture count
            *@return texture
            *}
            function GetTextureCount(): NativeInt; virtual;

            {**
            * Gets color
            *@return color
            *}
            function GetColor(): TQRColor; virtual;

            {**
            * Called when model texture should be loaded
            *@note This function is executed on the calling thread side
            *}
            procedure OnLoadTexture(); virtual;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            *}
            destructor Destroy(); override;

            {**
            * Processes the job
            *@returns true on success, otherwise false
            *}
            function Process(): Boolean; override;

            {**
            * Cancels the job
            *}
            procedure Cancel(); override;

            {**
            * Checks if job was canceled
            *@return true if job was canceled, otherwise false
            *}
            function IsCanceled(): Boolean; virtual;

            { Properties }
            property Model:                     TQRShapeModel read GetModel;
            property Texture[index: NativeInt]: TQRTexture    read GetTexture;
            property TextureCount:              NativeInt     read GetTextureCount;
            property Color:                     TQRColor      read GetColor;
    end;

    {**
    * Generic 3D shape group, contains all items and functions needed to manage a complete shape
    *}
    TQRShapeGroup = class(TQRStaticModelGroup)
        protected
            m_pJob: TQRShapeJob;

            {**
            * Gets dynamic mesh
            *@param[out] mesh - mesh
            *}
            procedure GetDynamicMesh(out mesh: TQRMesh); virtual;

            {**
            * Gets dynamic mesh, from cache if available, otherwise calculates and caches it
            *@param[out] mesh - mesh
            *@param[out] pTree - aligned-axis bounding box tree matching with mesh
            *}
            procedure GetDynamicMeshUseCache(out pMesh: PQRMesh; out pTree: TQRAABBTree); virtual;

            {**
            * Draws dynamic model
            *}
            procedure DrawDynamicModel(); virtual;

            {**
            * Draws cached model
            *}
            procedure DrawCachedModel(); virtual;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Clears script
            *}
            procedure Clear(); override;

            {**
            * Checks if group is empty
            *@return true if model is empty, otherwise false
            *}
            function IsEmpty(): Boolean; override;

            {**
            * Queries the job status
            *@return job status
            *}
            function QueryJobStatus(): TQRModelJobStatus; override;

            {**
            * Draws group
            *@param elapsedTime - elapsed time since last draw
            *}
            procedure Draw(const elapsedTime: Double); override;
    end;

    {**
    * Surface job
    *}
    TQRSurfaceJob = class(TQRShapeJob)
        protected
            {**
            * Called before a texture is loaded
            *@param pTexture - texture to load
            *@param custom - if true, texture is a custom user, otherwise a texture belonging to model
            *}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {**
            * Called when a known texture should be loaded
            *@param pTexture - texture to load
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param lengthX - surface length on x axis
            *@param lengthY - surface length on y axis
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup;
                     lengthX, lengthY: Single;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;
    end;

    {**
    * Surface group, contains all items and functions needed to manage a complete surface model
    *}
    TQRSurfaceGroup = class(TQRShapeGroup)
        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads surface group, creates and initializes model
            *@param lengthX - surface length on x axis
            *@param lengthY - surface length on y axis
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@return true on success, otherwise false
            *}
            function Load(lengthX, lengthY: Single;
                              const pColor: TQRColor;
                              modelOptions: TQRModelOptions): Boolean; virtual;
    end;

    {**
    * Box job
    *}
    TQRBoxJob = class(TQRShapeJob)
        protected
            {**
            * Called before a texture is loaded
            *@param pTexture - texture to load
            *@param custom - if true, texture is a custom user, otherwise a texture belonging to model
            *}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {**
            * Called when a known texture should be loaded
            *@param pTexture - texture to load
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param lengthX - box length on x axis
            *@param lengthY - box length on y axis
            *@param lengthZ - box length on z axis
            *@param pColor - model color
            *@param repeatTexOnEachFace - if true, texture will be repeated on each face
            *@param modelOptions - model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup;
            lengthX, lengthY, lengthZ: Single;
                         const pColor: TQRColor;
                  repeatTexOnEachFace: Boolean;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;
    end;

    {**
    * Box group, contains all items and functions needed to manage a complete box model
    *}
    TQRBoxGroup = class(TQRShapeGroup)
        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads box group, creates and initializes model
            *@param lengthX - box length on x axis
            *@param lengthY - box length on y axis
            *@param lengthZ - box length on z axis
            *@param pColor - model color
            *@param repeatTexOnEachFace - if true, texture will be repeated on each face
            *@param modelOptions - model options to apply
            *@return true on success, otherwise false
            *}
            function Load(lengthX, lengthY, lengthZ: Single;
                                       const pColor: TQRColor;
                                repeatTexOnEachFace: Boolean;
                                       modelOptions: TQRModelOptions): Boolean; virtual;
    end;

    {**
    * Sphere job
    *}
    TQRSphereJob = class(TQRShapeJob)
        protected
            {**
            * Called before a texture is loaded
            *@param pTexture - texture to load
            *@param custom - if true, texture is a custom user, otherwise a texture belonging to model
            *}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {**
            * Called when a known texture should be loaded
            *@param pTexture - texture to load
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param slices - sphere slices to generate
            *@param stacks - sphere stacks to generate
            *@param radius - sphere radius
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup;
                       slices, stacks: NativeUInt;
                               radius: Single;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;
    end;

    {**
    * Sphere group, contains all items and functions needed to manage a complete sphere model
    *}
    TQRSphereGroup = class(TQRShapeGroup)
        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads sphere group, creates and initializes model
            *@param slices - sphere slices to generate
            *@param stacks - sphere stacks to generate
            *@param radius - sphere radius
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@return true on success, otherwise false
            *}
            function Load(slices, stacks: NativeUInt;
                                  radius: Single;
                            const pColor: TQRColor;
                            modelOptions: TQRModelOptions): Boolean; virtual;
    end;

    {**
    * Cone job
    *}
    TQRConeJob = class(TQRShapeJob)
        protected
            {**
            * Called before a texture is loaded
            *@param pTexture - texture to load
            *@param custom - if true, texture is a custom user, otherwise a texture belonging to model
            *}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {**
            * Called when a known texture should be loaded
            *@param pTexture - texture to load
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param faceCount - number of faces composing the cone body (without top and bottom closing)
            *@param height - cone height
            *@param topRadiusX - x axis radius of the ellipse formed by the truncated apex, the
            *                    cone is not truncated if equals to 0.0
            *@param topRadiusY - y axis radius of the ellipse formed by the truncated apex, ignored
            *                    if topRadiusX is equals to 0.0
            *@param bottomRadiusX - x axis radius of the ellipse formed by the base
            *@param bottomRadiusY - y axis radius of the ellipse formed by the base
            *@param closing - cone closing type
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup;
                            faceCount: NativeUInt;
                               height,
                           topRadiusX,
                           topRadiusY,
                        bottomRadiusX,
                        bottomRadiusY: Single;
                              closing: EQR_Cone_Closing;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;
    end;

    {**
    * Cone group, contains all items and functions needed to manage a complete cone model
    *}
    TQRConeGroup = class(TQRShapeGroup)
        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads cone group, creates and initializes model
            *@param faceCount - number of faces composing the cone body (without top and bottom closing)
            *@param height - cone height
            *@param topRadiusX - x axis radius of the ellipse formed by the truncated apex, the
            *                    cone is not truncated if equals to 0.0
            *@param topRadiusY - y axis radius of the ellipse formed by the truncated apex, ignored
            *                    if topRadiusX is equals to 0.0
            *@param bottomRadiusX - x axis radius of the ellipse formed by the base
            *@param bottomRadiusY - y axis radius of the ellipse formed by the base
            *@param closing - cone closing type
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@return true on success, otherwise false
            *}
            function Load(faceCount: NativeUInt;
                             height,
                         topRadiusX,
                         topRadiusY,
                      bottomRadiusX,
                      bottomRadiusY: Single;
                            closing: EQR_Cone_Closing;
                       const pColor: TQRColor;
                       modelOptions: TQRModelOptions): Boolean; virtual;
    end;

    {**
    * Torus job
    *}
    TQRTorusJob = class(TQRShapeJob)
        protected
            {**
            * Called before a texture is loaded
            *@param pTexture - texture to load
            *@param custom - if true, texture is a custom user, otherwise a texture belonging to model
            *}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {**
            * Called when a known texture should be loaded
            *@param pTexture - texture to load
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param slices - torus slices
            *@param facesPerSlices - for each torus slice, the number of faces to build
            *@param outerRadiusX - outer radius on the (local) x axis
            *@param outerRadiusY - outer radius on the (local) y axis
            *@param innerRadiusX - inner radius on the (local) x axis
            *@param innerRadiusY - inner radius on the (local) y axis
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup;
                               slices,
                       facesPerSlices: NativeUInt;
                         outerRadiusX,
                         outerRadiusY,
                         innerRadiusX,
                         innerRadiusY: Single;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;
    end;

    {**
    * Torus group, contains all items and functions needed to manage a complete torus model
    *}
    TQRTorusGroup = class(TQRShapeGroup)
        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads cone group, creates and initializes model
            *@param slices - torus slices
            *@param facesPerSlices - for each torus slice, the number of faces to build
            *@param outerRadiusX - outer radius on the (local) x axis
            *@param outerRadiusY - outer radius on the (local) y axis
            *@param innerRadiusX - inner radius on the (local) x axis
            *@param innerRadiusY - inner radius on the (local) y axis
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@return true on success, otherwise false
            *}
            function Load(slices,
                  facesPerSlices: NativeUInt;
                    outerRadiusX,
                    outerRadiusY,
                    innerRadiusX,
                    innerRadiusY: Single;
                    const pColor: TQRColor;
                    modelOptions: TQRModelOptions): Boolean; virtual;
    end;

    {**
    * Parabola job
    *}
    TQRParabolaJob = class(TQRShapeJob)
        protected
            {**
            * Called before a texture is loaded
            *@param pTexture - texture to load
            *@param custom - if true, texture is a custom user, otherwise a texture belonging to model
            *}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {**
            * Called when a known texture should be loaded
            *@param pTexture - texture to load
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param slices - parabola slices
            *@param facesPerSlices - for each parabola slice, the number of faces to build
            *@param height - parabola height
            *@param radius - parabola radius
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup;
                               slices,
                       facesPerSlices: NativeUInt;
                       height, radius: Single;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;
    end;

    {**
    * Parabola group, contains all items and functions needed to manage a complete parabola model
    *}
    TQRParabolaGroup = class(TQRShapeGroup)
        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads cone group, creates and initializes model
            *@param slices - parabola slices
            *@param facesPerSlices - for each parabola slice, the number of faces to build
            *@param height - parabola height
            *@param radius - parabola radius
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@return true on success, otherwise false
            *}
            function Load(slices, facesPerSlices: NativeUInt;
                                  height, radius: Single;
                                    const pColor: TQRColor;
                                    modelOptions: TQRModelOptions): Boolean; virtual;
    end;

implementation
//------------------------------------------------------------------------------
// TQRShapeHelper
//------------------------------------------------------------------------------
class procedure TQRShapeHelper.AddVertex(const pPosition, pNormal: PQRVector3D;
                                                  const pTexCoord: PQRVector2D;
                                                     const pColor: TQRColor;
                                                        var index: NativeUInt;
                                                       var vertex: TQRVertex);
begin
    // initialize memory to add vertex in buffer
    SetLength(vertex.m_Buffer, NativeUInt(Length(vertex.m_Buffer)) + vertex.m_Stride);

    // set position
    vertex.m_Buffer[index]     := pPosition.X;
    vertex.m_Buffer[index + 1] := pPosition.Y;
    vertex.m_Buffer[index + 2] := pPosition.Z;

    Inc(index, 3);

    // do generate normals?
    if (EQR_VF_Normals in vertex.m_Format) then
    begin
        // set normal
        vertex.m_Buffer[index]     := pNormal.X;
        vertex.m_Buffer[index + 1] := pNormal.Y;
        vertex.m_Buffer[index + 2] := pNormal.Z;

        Inc(index, 3);
    end;

    // do generate texture coordinates?
    if (EQR_VF_TexCoords in vertex.m_Format) then
    begin
        // add texture coordinates data to buffer
        vertex.m_Buffer[index]     := pTexCoord.X;
        vertex.m_Buffer[index + 1] := pTexCoord.Y;

        Inc(index, 2);
    end;

    // do generate colors?
    if (EQR_VF_Colors in vertex.m_Format) then
    begin
        // set color data
        vertex.m_Buffer[index]     := pColor.GetRedF;
        vertex.m_Buffer[index + 1] := pColor.GetGreenF;
        vertex.m_Buffer[index + 2] := pColor.GetBlueF;
        vertex.m_Buffer[index + 3] := pColor.GetAlphaF;

        Inc(index, 4);
    end;
end;
//------------------------------------------------------------------------------
// TQRShapeJob
//------------------------------------------------------------------------------
constructor TQRShapeJob.Create(pGroup: TQRModelGroup;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent);
begin
    inherited Create(pGroup, modelOptions);

    // create local variables
    m_MaxTexture    := 100;
    m_TextureLoaded := False;
    m_IsCanceled    := False;

    // copy values needed to load the model
    m_pColor         := TQRColor.Create(pColor);
    m_fOnLoadTexture := fOnLoadTexture;
end;
//------------------------------------------------------------------------------
destructor TQRShapeJob.Destroy();
var
    i: NativeUInt;
begin
    m_pLock.Lock;

    try
        // clear textures
        if (Length(m_Textures) > 0) then
            for i := 0 to Length(m_Textures) - 1 do
                m_Textures[i].Free;

        SetLength(m_Textures, 0);

        // clear memory
        m_pModel.Free;
        m_pColor.Free;
    finally
        m_pLock.Unlock;
    end;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRShapeJob.GetModel(): TQRShapeModel;
begin
    m_pLock.Lock;
    Result := m_pModel;
    m_pLock.Unlock;
end;
//------------------------------------------------------------------------------
function TQRShapeJob.GetTexture(index: NativeInt): TQRTexture;
begin
    m_pLock.Lock;

    if (index >= Length(m_Textures)) then
    begin
        Result := nil;
        Exit;
    end;

    Result := m_Textures[index];

    m_pLock.Unlock;
end;
//------------------------------------------------------------------------------
function TQRShapeJob.GetTextureCount(): NativeInt;
begin
    m_pLock.Lock;
    Result := Length(m_Textures);
    m_pLock.Unlock;
end;
//------------------------------------------------------------------------------
function TQRShapeJob.GetColor(): TQRColor;
begin
    m_pLock.Lock;
    Result := m_pColor;
    m_pLock.Unlock;
end;
//------------------------------------------------------------------------------
procedure TQRShapeJob.OnLoadTexture();
var
    textureIndex: NativeInt;
    max:          NativeUInt;
    loadNext:     Boolean;
begin
    m_pLock.Lock;

    try
        m_TextureLoaded := False;

        // load texture
        if (Assigned(m_fOnLoadTexture)) then
        begin
            loadNext := True;
            max      := 0;

            // do load next texture, until no more texture should be loaded
            while loadNext do
            begin
                // add a texture to list
                textureIndex := Length(m_Textures);
                SetLength(m_Textures, textureIndex + 1);
                m_Textures[textureIndex] := TQRTexture.Create;

                BeforeLoadTexture(m_Textures[textureIndex], False);

                loadNext := False;

                // load texture
                if (not m_fOnLoadTexture(m_pGroup,
                                         m_pModel,
                                         nil,
                                         m_Textures[textureIndex],
                                         loadNext))
                then
                    Exit;

                Inc(max);

                // too many textures were loaded?
                if (max >= m_MaxTexture) then
                    raise Exception.Create('Too many textures were created');
            end;

            m_TextureLoaded := True;
        end;
    finally
        m_pLock.Unlock;
    end;
end;
//------------------------------------------------------------------------------
function TQRShapeJob.Process(): Boolean;
var
    textureLoaded:           Boolean;
    vertexFormat:            TQRVertexFormat;
    pMesh:                   PQRMesh;
    pTree:                   TQRAABBTree;
    progressStep, totalStep: Single;
    doCreateCache:           Boolean;
begin
    // if job was still loaded, don't reload it. A such scenario can happen when a job is deleted in
    // the job list. In this case, all jobs are removed from list, the concerned job is deleted,
    // then all remaining jobs are added back, calling thus the Process() function again
    if (m_IsLoaded) then
    begin
        Result := True;
        Exit;
    end;

    try
        // check if cache should be created
        doCreateCache := ((EQR_MO_Create_Cache   in m_ModelOptions) and
                      not (EQR_MO_Dynamic_Frames in m_ModelOptions));

        // do create cache?
        if (doCreateCache) then
            // calculate step count
            totalStep := 3.0
        else
            // get step count
            totalStep := 2.0;

        // calculate the value of a progress step
        progressStep := (100.0 / totalStep);

        // populate model options
        m_pModel.Color := m_pColor;

        // notify main interface that texture should be loaded, wait until function returns
        TThread.Synchronize(nil, OnLoadTexture);

        // textures are loaded, add one step to progress
        m_Progress := m_Progress + progressStep;

        m_pLock.Lock;
        textureLoaded := m_TextureLoaded;
        m_pLock.Unlock;

        // do include colors?
        if (EQR_MO_Without_Colors in m_ModelOptions) then
            vertexFormat := []
        else
            vertexFormat := [EQR_VF_Colors];

        // normals loaded?
        if (not(EQR_MO_Without_Normals in m_ModelOptions)) then
            Include(vertexFormat, EQR_VF_Normals);

        // texture loaded?
        if (textureLoaded and (not(EQR_MO_Without_Textures in m_ModelOptions))) then
            Include(vertexFormat, EQR_VF_TexCoords);

        // set vertex format
        m_pModel.VertexFormat := vertexFormat;

        // model is configured, add one step to progress
        m_Progress := m_Progress + progressStep;

        // do not create cache?
        if (not doCreateCache) then
        begin
            m_IsLoaded := True;
            Result     := True;
            Exit;
        end;

        // create mesh
        New(pMesh);

        // do ignore collisions?
        if (not(EQR_MO_No_Collision in m_ModelOptions)) then
            // create AABB tree
            pTree := TQRAABBTree.Create
        else
            pTree := nil;

        // get current mesh
        if (not m_pModel.GetMesh(pMesh^, pTree, IsCanceled)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('Shape frame creation failed or was canceled - class name - ' +
                                           ClassName);
            {$endif}

            // failed or canceled?
            Dispose(pMesh);
            pTree.Free;
            Result := False;
            Exit;
        end;

        // add mesh to cache, note that from now cache will take care of the pointer
        try
            m_pCache.Mesh[0] := pMesh;
        except
            Dispose(pMesh);
        end;

        // do ignore collisions?
        if (not(EQR_MO_No_Collision in m_ModelOptions)) then
            // add tree to cache, note that from now cache will take care of the pointer
            try
                m_pCache.AABBTree[0] := pTree;
            except
                pTree.Free;
            end;

        // cache was created, add one step to progress
        m_Progress := m_Progress + progressStep;

        m_IsLoaded := True;
        Result     := True;
    finally
        TThread.Synchronize(nil, OnAfterLoadModel);
    end;
end;
//------------------------------------------------------------------------------
procedure TQRShapeJob.Cancel();
begin
    m_pLock.Lock;
    m_IsCanceled := True;
    m_pLock.Unlock;
end;
//------------------------------------------------------------------------------
function TQRShapeJob.IsCanceled(): Boolean;
begin
    m_pLock.Lock;
    Result := m_IsCanceled;
    m_pLock.Unlock;
end;
//------------------------------------------------------------------------------
// TQRShapeGroup
//------------------------------------------------------------------------------
constructor TQRShapeGroup.Create();
begin
    inherited Create;
end;
//------------------------------------------------------------------------------
destructor TQRShapeGroup.Destroy();
begin
    // delete model and his associated job, don't forget to unregister it from worker
    if (Assigned(m_pJob)) then
    begin
        TQRModelWorker.GetInstance.CancelJob(m_pJob);
        m_pJob := nil;
    end;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRShapeGroup.Clear();
begin
    // previous job was created?
    if (Assigned(m_pJob)) then
    begin
        // delete previous job
        TQRModelWorker.GetInstance.CancelJob(m_pJob);
        m_pJob := nil;
    end;
end;
//------------------------------------------------------------------------------
function TQRShapeGroup.IsEmpty(): Boolean;
begin
    Result := (not Assigned(m_pJob));
end;
//------------------------------------------------------------------------------
procedure TQRShapeGroup.GetDynamicMesh(out mesh: TQRMesh);
begin
    // get mesh
    if (not m_pJob.Model.GetMesh(mesh, TQRAABBTree(nil), TQRIsCanceledEvent(nil)))
    then
    begin
        {$ifdef DEBUG}
            TQRLogHelper.LogToCompiler('Shape frame creation failed - class name - ' + ClassName);
        {$endif}
    end;
end;
//------------------------------------------------------------------------------
procedure TQRShapeGroup.GetDynamicMeshUseCache(out pMesh: PQRMesh; out pTree: TQRAABBTree);
var
    useCollisions: Boolean;
begin
    pMesh := nil;
    pTree := nil;

    useCollisions := not (EQR_MO_No_Collision in m_pJob.ModelOptions);

    // get mesh from cache
    pMesh := m_pJob.Mesh[0];

    // do create collision buffers?
    if (useCollisions) then
        // get AABB tree from cache
        pTree := m_pJob.AABBTree[0];

    // found in cache?
    if (Assigned(pMesh) and ((not useCollisions) or Assigned(pTree))) then
        Exit;

    // create new mesh
    New(pMesh);

    // do create collision buffers?
    if (useCollisions) then
        // create new AABB tree
        pTree := TQRAABBTree.Create;

    // get mesh and calculate AABB tree, if needed
    if (not m_pJob.Model.GetMesh(pMesh^, pTree, TQRIsCanceledEvent(nil)))
    then
    begin
        {$ifdef DEBUG}
            TQRLogHelper.LogToCompiler('Shape model frame creation failed - class name - ' + ClassName);
        {$endif}

        // failed?
        Dispose(pMesh);
        pTree.Free;
        Exit;
    end;

    // add meshes to cache, note that from now cache will take care of the pointer
    try
        m_pJob.Mesh[0] := pMesh;
    except
        Dispose(pMesh);
    end;

    // do create collision buffers?
    if (useCollisions) then
        // add tree to cache, note that from now cache will take care of the pointer
        try
            m_pJob.AABBTree[0] := pTree;
        except
            pTree.Free;
        end;
end;
//------------------------------------------------------------------------------
procedure TQRShapeGroup.DrawDynamicModel();
var
    pMesh: PQRMesh;
    pTree: TQRAABBTree;
begin
    // nothing to draw?
    if (not Assigned(m_fOnDrawItem)) then
        Exit;

    // can use dynamic cache?
    if (EQR_MO_Dynamic_Frames_No_Cache in m_pJob.ModelOptions) then
    begin
        try
            // create meshes to draw
            New(pMesh);

            // get mesh
            GetDynamicMesh(pMesh^);

            // draw mesh
            m_fOnDrawItem(Self, m_pJob.Model, m_pJob.m_Textures, GetMatrix(), pMesh, nil);
        finally
            // clear memory
            Dispose(pMesh);
        end;

        Exit;
    end;

    // get meshes and AABB trees from cache, create them if still not exist
    GetDynamicMeshUseCache(pMesh, pTree);

    // draw mesh
    m_fOnDrawItem(Self, m_pJob.Model, m_pJob.m_Textures, GetMatrix(), pMesh, pTree);
end;
//------------------------------------------------------------------------------
procedure TQRShapeGroup.DrawCachedModel();
begin
    // nothing to draw?
    if (not Assigned(m_fOnDrawItem)) then
        Exit;

    // collision buffers were created?
    if (EQR_MO_No_Collision in m_pJob.ModelOptions) then
        // draw mesh, ignore collisions
        m_fOnDrawItem(Self, m_pJob.Model, m_pJob.m_Textures, GetMatrix(), m_pJob.Mesh[0], nil)
    else
        // draw mesh
        m_fOnDrawItem(Self,
                      m_pJob.Model,
                      m_pJob.m_Textures,
                      GetMatrix(),
                      m_pJob.Mesh[0],
                      m_pJob.AABBTree[0]);
end;
//------------------------------------------------------------------------------
function TQRShapeGroup.QueryJobStatus(): TQRModelJobStatus;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
    begin
        // set default values
        m_pJobStatus.Status   := EQR_JS_NotStarted;
        m_pJobStatus.Progress := 0;
    end
    else
    begin
        // get status from running job
        m_pJobStatus.Status   := m_pJob.GetStatus;
        m_pJobStatus.Progress := Floor(m_pJob.Progress);
    end;

    Result := m_pJobStatus;
end;
//------------------------------------------------------------------------------
procedure TQRShapeGroup.Draw(const elapsedTime: Double);
begin
    // model not created?
    if (not Assigned(m_pJob)) then
        Exit;

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
        Exit;

    // can draw model from a previously built cache, do generate frames dynamically, or let user
    // take care of frames creation?
    if (EQR_MO_Create_Cache in m_pJob.ModelOptions) then
        DrawCachedModel()
    else
    if ((EQR_MO_Dynamic_Frames          in m_pJob.ModelOptions) or
        (EQR_MO_Dynamic_Frames_No_Cache in m_pJob.ModelOptions))
    then
        DrawDynamicModel()
    else
    if (Assigned(m_fOnCustomDrawItem))
    then
        // let user take care of drawing model
        m_fOnCustomDrawItem(Self,
                            m_pJob.Model,
                            m_pJob.m_Textures,
                            GetMatrix());
end;
//------------------------------------------------------------------------------
// TQRSurfaceJob
//------------------------------------------------------------------------------
constructor TQRSurfaceJob.Create(pGroup: TQRModelGroup;
                       lengthX, lengthY: Single;
                           const pColor: TQRColor;
                           modelOptions: TQRModelOptions;
                         fOnLoadTexture: TQRLoadMeshTextureEvent);
var
    pSurfaceModel: TQRSurfaceModel;
begin
    inherited Create(pGroup, pColor, modelOptions, fOnLoadTexture);

    // create local variables
    pSurfaceModel         := TQRSurfaceModel.Create;
    pSurfaceModel.LengthX := lengthX;
    pSurfaceModel.LengthY := lengthY;
    m_pModel              := pSurfaceModel;
end;
//------------------------------------------------------------------------------
destructor TQRSurfaceJob.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRSurfaceJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_surface';
end;
//------------------------------------------------------------------------------
function TQRSurfaceJob.LoadTexture(pTexture: TQRTexture;
                                    pBitmap: Vcl.Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//------------------------------------------------------------------------------
// TQRSurfaceGroup
//------------------------------------------------------------------------------
constructor TQRSurfaceGroup.Create();
begin
    inherited Create;

    m_pJob := nil;
end;
//------------------------------------------------------------------------------
destructor TQRSurfaceGroup.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRSurfaceGroup.Load(lengthX, lengthY: Single;
                                  const pColor: TQRColor;
                                  modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear();

    // prepare model job to load from file
    m_pJob := TQRSurfaceJob.Create(Self,
                                   lengthX,
                                   lengthY,
                                   pColor,
                                   modelOptions,
                                   m_fOnLoadTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//------------------------------------------------------------------------------
// TQRBoxJob
//------------------------------------------------------------------------------
constructor TQRBoxJob.Create(pGroup: TQRModelGroup;
          lengthX, lengthY, lengthZ: Single;
                       const pColor: TQRColor;
                repeatTexOnEachFace: Boolean;
                       modelOptions: TQRModelOptions;
                     fOnLoadTexture: TQRLoadMeshTextureEvent);
var
    pBoxModel: TQRBoxModel;
begin
    inherited Create(pGroup, pColor, modelOptions, fOnLoadTexture);

    // create local variables
    pBoxModel                     := TQRBoxModel.Create;
    pBoxModel.LengthX             := lengthX;
    pBoxModel.LengthY             := lengthY;
    pBoxModel.LengthZ             := lengthZ;
    pBoxmodel.RepeatTexOnEachFace := repeatTexOnEachFace;
    m_pModel                      := pBoxModel;
end;
//------------------------------------------------------------------------------
destructor TQRBoxJob.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRBoxJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_box';
end;
//------------------------------------------------------------------------------
function TQRBoxJob.LoadTexture(pTexture: TQRTexture;
                                pBitmap: Vcl.Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//------------------------------------------------------------------------------
// TQRBoxGroup
//------------------------------------------------------------------------------
constructor TQRBoxGroup.Create();
begin
    inherited Create;

    m_pJob := nil;
end;
//------------------------------------------------------------------------------
destructor TQRBoxGroup.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRBoxGroup.Load(lengthX, lengthY, lengthZ: Single;
                                       const pColor: TQRColor;
                                repeatTexOnEachFace: Boolean;
                                       modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear();

    // prepare model job to load from file
    m_pJob := TQRBoxJob.Create(Self,
                               lengthX,
                               lengthY,
                               lengthZ,
                               pColor,
                               repeatTexOnEachFace,
                               modelOptions,
                               m_fOnLoadTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//------------------------------------------------------------------------------
// TQRSphereJob
//------------------------------------------------------------------------------
constructor TQRSphereJob.Create(pGroup: TQRModelGroup;
                        slices, stacks: NativeUInt;
                                radius: Single;
                          const pColor: TQRColor;
                          modelOptions: TQRModelOptions;
                        fOnLoadTexture: TQRLoadMeshTextureEvent);
var
    pSphereModel: TQRSphereModel;
begin
    inherited Create(pGroup, pColor, modelOptions, fOnLoadTexture);

    // create local variables
    pSphereModel        := TQRSphereModel.Create;
    pSphereModel.Slices := slices;
    pSphereModel.Stacks := stacks;
    pSphereModel.Radius := radius;
    m_pModel            := pSphereModel;
end;
//------------------------------------------------------------------------------
destructor TQRSphereJob.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRSphereJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_sphere';
end;
//------------------------------------------------------------------------------
function TQRSphereJob.LoadTexture(pTexture: TQRTexture;
                                   pBitmap: Vcl.Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//------------------------------------------------------------------------------
// TQRSphereGroup
//------------------------------------------------------------------------------
constructor TQRSphereGroup.Create();
begin
    inherited Create;

    m_pJob := nil;
end;
//------------------------------------------------------------------------------
destructor TQRSphereGroup.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRSphereGroup.Load(slices, stacks: NativeUInt;
                                     radius: Single;
                               const pColor: TQRColor;
                               modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear();

    // prepare model job to load from file
    m_pJob := TQRSphereJob.Create(Self,
                                  slices,
                                  stacks,
                                  radius,
                                  pColor,
                                  modelOptions,
                                  m_fOnLoadTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//------------------------------------------------------------------------------
// TQRConeJob
//------------------------------------------------------------------------------
constructor TQRConeJob.Create(pGroup: TQRModelGroup;
                           faceCount: NativeUInt;
                              height,
                          topRadiusX,
                          topRadiusY,
                       bottomRadiusX,
                       bottomRadiusY: Single;
                             closing: EQR_Cone_Closing;
                        const pColor: TQRColor;
                        modelOptions: TQRModelOptions;
                      fOnLoadTexture: TQRLoadMeshTextureEvent);
var
    pConeModel: TQRConeModel;
begin
    inherited Create(pGroup, pColor, modelOptions, fOnLoadTexture);

    // create local variables
    pConeModel               := TQRConeModel.Create;
    pConeModel.FaceCount     := faceCount;
    pConeModel.Height        := height;
    pConeModel.TopRadiusX    := topRadiusX;
    pConeModel.TopRadiusY    := topRadiusY;
    pConeModel.BottomRadiusX := bottomRadiusX;
    pConeModel.BottomRadiusY := bottomRadiusY;
    pConeModel.Closing       := closing;
    m_pModel                 := pConeModel;
end;
//------------------------------------------------------------------------------
destructor TQRConeJob.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRConeJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_cone';
end;
//------------------------------------------------------------------------------
function TQRConeJob.LoadTexture(pTexture: TQRTexture;
                                 pBitmap: Vcl.Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//------------------------------------------------------------------------------
// TQRConeGroup
//------------------------------------------------------------------------------
constructor TQRConeGroup.Create();
begin
    inherited Create;

    m_pJob := nil;
end;
//------------------------------------------------------------------------------
destructor TQRConeGroup.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRConeGroup.Load(faceCount: NativeUInt;
                              height,
                          topRadiusX,
                          topRadiusY,
                       bottomRadiusX,
                       bottomRadiusY: Single;
                             closing: EQR_Cone_Closing;
                        const pColor: TQRColor;
                        modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear();

    // prepare model job to load from file
    m_pJob := TQRConeJob.Create(Self,
                                faceCount,
                                height,
                                topRadiusX,
                                topRadiusY,
                                bottomRadiusX,
                                bottomRadiusY,
                                closing,
                                pColor,
                                modelOptions,
                                m_fOnLoadTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//------------------------------------------------------------------------------
// TQRTorusJob
//------------------------------------------------------------------------------
constructor TQRTorusJob.Create(pGroup: TQRModelGroup;
                               slices,
                       facesPerSlices: NativeUInt;
                         outerRadiusX,
                         outerRadiusY,
                         innerRadiusX,
                         innerRadiusY: Single;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent);
var
    pTorusModel: TQRTorusModel;
begin
    inherited Create(pGroup, pColor, modelOptions, fOnLoadTexture);

    // create local variables
    pTorusModel                := TQRTorusModel.Create;
    pTorusModel.Slices         := slices;
    pTorusModel.FacesPerSlices := facesPerSlices;
    pTorusModel.OuterRadiusX   := outerRadiusX;
    pTorusModel.OuterRadiusY   := outerRadiusY;
    pTorusModel.InnerRadiusX   := innerRadiusX;
    pTorusModel.InnerRadiusY   := innerRadiusY;
    m_pModel                   := pTorusModel;
end;
//------------------------------------------------------------------------------
destructor TQRTorusJob.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRTorusJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_torus';
end;
//------------------------------------------------------------------------------
function TQRTorusJob.LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//------------------------------------------------------------------------------
// TQRTorusGroup
//------------------------------------------------------------------------------
constructor TQRTorusGroup.Create();
begin
    inherited Create;

    m_pJob := nil;
end;
//------------------------------------------------------------------------------
destructor TQRTorusGroup.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRTorusGroup.Load(slices,
                    facesPerSlices: NativeUInt;
                      outerRadiusX,
                      outerRadiusY,
                      innerRadiusX,
                      innerRadiusY: Single;
                      const pColor: TQRColor;
                      modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear();

    // prepare model job to load from file
    m_pJob := TQRTorusJob.Create(Self,
                                 slices,
                                 facesPerSlices,
                                 outerRadiusX,
                                 outerRadiusY,
                                 innerRadiusX,
                                 innerRadiusY,
                                 pColor,
                                 modelOptions,
                                 m_fOnLoadTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//------------------------------------------------------------------------------
// TQRParabolaJob
//------------------------------------------------------------------------------
constructor TQRParabolaJob.Create(pGroup: TQRModelGroup;
                                  slices,
                          facesPerSlices: NativeUInt;
                          height, radius: Single;
                            const pColor: TQRColor;
                            modelOptions: TQRModelOptions;
                          fOnLoadTexture: TQRLoadMeshTextureEvent);
var
    pParabolaModel: TQRParabolaModel;
begin
    inherited Create(pGroup, pColor, modelOptions, fOnLoadTexture);

    // create local variables
    pParabolaModel                := TQRParabolaModel.Create;
    pParabolaModel.Slices         := slices;
    pParabolaModel.FacesPerSlices := facesPerSlices;
    pParabolaModel.Height         := height;
    pParabolaModel.Radius         := radius;
    m_pModel                      := pParabolaModel;
end;
//------------------------------------------------------------------------------
destructor TQRParabolaJob.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRParabolaJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_parabola';
end;
//------------------------------------------------------------------------------
function TQRParabolaJob.LoadTexture(pTexture: TQRTexture;
                                     pBitmap: Vcl.Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//------------------------------------------------------------------------------
// TQRParabolaGroup
//------------------------------------------------------------------------------
constructor TQRParabolaGroup.Create();
begin
    inherited Create;

    m_pJob := nil;
end;
//------------------------------------------------------------------------------
destructor TQRParabolaGroup.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRParabolaGroup.Load(slices, facesPerSlices: NativeUInt;
                                       height, radius: Single;
                                         const pColor: TQRColor;
                                         modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear();

    // prepare model job to load from file
    m_pJob := TQRParabolaJob.Create(Self,
                                    slices,
                                    facesPerSlices,
                                    height,
                                    radius,
                                    pColor,
                                    modelOptions,
                                    m_fOnLoadTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//------------------------------------------------------------------------------

end.
