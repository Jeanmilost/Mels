{**************************************************************************************************
 * ==> UTQRModel ---------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides the tools to load a model and build his vertex buffer       *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRModel;

interface

uses System.Classes,
     System.SysUtils,
     UTQRCommon,
     UTQRCache,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision;

type
    {**
    * Model helper
    *}
    TQRModelHelper = class
        public
            {**
            * Checks if there is enough remaining space in buffer to read next data
            *@param pBuffer - buffer in which data should be read
            *@param lengthToRead - data length to read in buffer
            *@param[out] errorMsg - on error, contains an error message to show
            *@return true if next data can be read in buffer, otherwise false
            *}
            class function ValidateNextRead(pBuffer: TStream;
                                       lengthToRead: NativeUInt;
                                       out errorMsg: UnicodeString): Boolean; static;

            {**
            * Populate aligned-axis bounding box tree
            *@param mesh - source mesh from which aligned-axis bounding box tree should be populated
            *@param pAABBTree - aligned-axis bounding box tree to populate, ignored if nil
            *@param hIsCanceled - is canceled callback function to use, ignored if nil
            *@return true on success, otherwise false
            *}
            class function PopulateAABBTree(const mesh: TQRMesh;
                                             pAABBTree: TQRAABBTree;
                                           hIsCanceled: TQRIsCanceledEvent = nil): Boolean; static;

            {**
            * Interpolates mesh
            *@param position - interpolation position, in percent (between 0.0f and 1.0f)
            *@param mesh1 - first mesh to interpolate
            *@param mesh2 - second mesh to interpolate
            *@param[out] mesh -resulting interpolated mesh
            *@return true on success, otherwise false
            *@note This function should only be used for compatibility with old OpenGL 1.x versions,
            *      as normally interpolation should be done in vertex shader
            *}
            class function Interpolate(const position: Single;
                                   const mesh1, mesh2: TQRMesh;
                                             out mesh: TQRMesh): Boolean; static;
    end;

    {**
    * Model cache item, cache all data needed to render a model, detect model collisions, ...
    *}
    TQRModelCache = class
        protected
            m_pMeshCache:     TQRCache<NativeUInt, PQRMesh>;
            m_pAABBTreeCache: TQRCache<NativeUInt, TQRAABBTree>;

            {**
            * Called when mesh is deleted from cache
            *@param[in, out] key - deleting key
            *@param[in, out] pMesh - deleting value
            *@return true if value can be deleted from cache, otherwise false
            *}
            function OnDeleteMesh(const key: NativeUInt; var pMesh: PQRMesh): Boolean; virtual;

            {**
            * Called when aligned-axis bounding box tree is deleted from cache
            *@param[in, out] key - deleting key
            *@param[in, out] pTree - deleting value
            *@return true if value can be deleted from cache, otherwise false
            *}
            function OnDeleteAABBTree(const key: NativeUInt; var pTree: TQRAABBTree): Boolean; virtual;

            {**
            * Gets mesh
            *@return mesh
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
            *@return tree
            *}
            function GetTree(i: NativeUInt): TQRAABBTree; virtual;

            {**
            * Sets aligned-axis bounding box tree
            *@param pTree - tree
            *@note Be careful, the internal cache will take the tree ownership, so don't try to
            *      delete it externally
            *}
            procedure SetTree(i: NativeUInt; pTree: TQRAABBTree); virtual;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            { Properties }
            property Mesh[i: NativeUInt]:     PQRMesh     read GetMesh write SetMesh;
            property AABBTree[i: NativeUInt]: TQRAABBTree read GetTree write SetTree;
    end;

    {**
    * Basic model parser
    *}
    TQRModelParser = class
        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Loads model from file
            *@param fileName - file name
            *@return true on success, otherwise false
            *}
            function Load(const fileName: TFileName): Boolean; overload; virtual; abstract;

            {**
            * Loads model from buffer
            *@param pBuffer - buffer
            *@param length - length to read in buffer, in bytes
            *@return true on success, otherwise false
            *@note Read will begin from current offset
            *}
            function Load(const pBuffer: TStream;
                                 length: NativeUInt): Boolean; overload; virtual; abstract;
    end;

    {**
    * Basic 3D model
    *}
    TQRModel = class
        protected
            m_pColor:       TQRColor;
            m_VertexFormat: TQRVertexFormat;

            {**
            * Gets model color
            *@return model color
            *}
            function GetColor(): TQRColor; virtual;

            {**
            * Sets model color
            *@param pColor - model color to set
            *}
            procedure SetColor(const pColor: TQRColor); virtual;

            {**
            * Gets vertex format
            *@return vertex format
            *}
            function GetVertexFormat(): TQRVertexFormat; virtual;

            {**
            * Gets vertex format
            *@param value - vertex format to set
            *}
            procedure SetVertexFormat(value: TQRVertexFormat); virtual;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            { Properties }
            property Color:        TQRColor        read GetColor        write SetColor;
            property VertexFormat: TQRVertexFormat read GetVertexFormat write SetVertexFormat;
    end;

    {**
    * Basic 3D static model. A static model is a simple model without animation, as e.g. a sphere,
    * or a frontwave model
    *}
    TQRStaticModel = class(TQRModel)
        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Gets model mesh
            *@param[out] mesh - mesh
            *@param pAABBTree - aligned-axis bounding box tree to populate, ignored if nil
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *}
            function GetMesh(out mesh: TQRMesh;
                            pAABBTree: TQRAABBTree;
                          hIsCanceled: TQRIsCanceledEvent): Boolean; virtual; abstract;
    end;

    {**
    * Basic 3D framed model. A framed model is a model in which the animation is done frame by frame,
    * as e.g. a md2 model
    *}
    TQRFramedModel = class(TQRModel)
        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Gets model frame mesh
            *@param index - frame mesh index to create
            *@param[out] mesh - frame mesh
            *@param pAABBTree - aligned-axis bounding box tree to populate, ignored if nil
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *}
            function GetMesh(index: NativeUInt;
                          out mesh: TQRMesh;
                         pAABBTree: TQRAABBTree;
                       hIsCanceled: TQRIsCanceledEvent): Boolean; overload; virtual; abstract;

            {**
            * Gets model frame mesh
            *@param index - frame mesh index to get
            *@param nextIndex - frame mesh index to interpolate with
            *@param interpolationFactor - interpolation factor to apply
            *@param[out] mesh - frame mesh
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *}
            function GetMesh(index, nextIndex: NativeUInt;
                          interpolationFactor: Double;
                                     out mesh: TQRMesh;
                                  hIsCanceled: TQRIsCanceledEvent): Boolean; overload; virtual; abstract;

            {**
            * Gets mesh count
            *@return mesh count
            *}
            function GetMeshCount(): NativeUInt; virtual; abstract;
    end;

    {**
    * Basic 3D articulated model. An articulated model is a model in which animation is based on
    * bones, or any kind of articulable structure, as e.g. 3ds models, x models or md5 models
    *}
    TQRArticulatedModel = class(TQRModel)
        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;
    end;

implementation
//------------------------------------------------------------------------------
// TQRModelHelper
//------------------------------------------------------------------------------
class function TQRModelHelper.ValidateNextRead(pBuffer: TStream;
                                          lengthToRead: NativeUInt;
                                          out errorMsg: UnicodeString): Boolean;
begin
    // check if buffer is large enough to read next data block
    if ((pBuffer.Position + lengthToRead) <= pBuffer.Size) then
    begin
        Result := True;
        Exit;
    end;

    // build error message containing debug informations
    errorMsg := 'offset - '              + IntToStr(pBuffer.Position) +
                ' - remaining length - ' + IntToStr(pBuffer.Size - pBuffer.Position) +
                ' - length to read - '   + IntToStr(lengthToRead);

    Result := False;
end;
//------------------------------------------------------------------------------
class function TQRModelHelper.PopulateAABBTree(const mesh: TQRMesh;
                                                pAABBTree: TQRAABBTree;
                                              hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    meshCount, i: NativeUInt;
    polygons:     TQRPolygons;
begin
    // no destination tree?
    if (not Assigned(pAABBTree)) then
    begin
        // it's not an error so return true
        Result := True;
        Exit;
    end;

    // get mesh count
    meshCount := Length(mesh);

    // no source mesh?
    if (meshCount = 0) then
    begin
        // it's not an error so return true
        Result := True;
        Exit;
    end;

    // iterate through meshes
    for i := 0 to meshCount - 1 do
        // get collide polygons
        if (not TQRCollisionHelper.GetPolygons(mesh[i], polygons, hIsCanceled)) then
        begin
            Result := False;
            Exit;
        end;

    // populate aligned-axis bounding box tree
    Result := pAABBTree.Populate(polygons, hIsCanceled);
end;
//------------------------------------------------------------------------------
class function TQRModelHelper.Interpolate(const position: Single;
                                      const mesh1, mesh2: TQRMesh;
                                                out mesh: TQRMesh): Boolean;
var
    count, bufferCount, i, j, index:                   NativeInt;
    vertex:                                            TQRVertex;
    srcVec, dstVec, vec, srcNormal, dstNormal, normal: TQRVector3D;
begin
    // get vertice count
    count := Length(mesh1);

    // are mesh compatible?
    if (count <> Length(mesh2)) then
    begin
        Result := False;
        Exit;
    end;

    // is mesh empty?
    if (count = 0) then
    begin
        Result := False;
        Exit;
    end;

    // iterate through mesh to interpolate
    for i := 0 to count - 1 do
    begin
        // are frame compatibles?
        if (not mesh1[i].CompareFormat(mesh2[i])) then
        begin
            Result := False;
            Exit;
        end;

        // not a 3D coordinate?
        if (mesh1[i].m_CoordType <> EQR_VC_XYZ) then
        begin
            Result := False;
            Exit;
        end;

        vertex.m_Name      := mesh1[i].m_Name;
        vertex.m_Stride    := mesh1[i].m_Stride;
        vertex.m_Type      := mesh1[i].m_Type;
        vertex.m_Format    := mesh1[i].m_Format;
        vertex.m_CoordType := mesh1[i].m_CoordType;

        // get vertex buffer data count
        bufferCount := Length(mesh1[i].m_Buffer);

        j := 0;

        // iterate through vertex buffer content
        while (j < bufferCount) do
        begin
            index := 3;

            // get positions
            srcVec.X := mesh1[i].m_Buffer[j];
            srcVec.Y := mesh1[i].m_Buffer[j + 1];
            srcVec.Z := mesh1[i].m_Buffer[j + 2];
            dstVec.X := mesh2[i].m_Buffer[j];
            dstVec.Y := mesh2[i].m_Buffer[j + 1];
            dstVec.Z := mesh2[i].m_Buffer[j + 2];

            // interpolate positions
            vec := srcVec.Interpolate(dstVec, position);

            // set interpolated positions in destination buffer
            SetLength(vertex.m_Buffer, Length(vertex.m_Buffer) + 3);
            vertex.m_Buffer[j]     := vec.X;
            vertex.m_Buffer[j + 1] := vec.Y;
            vertex.m_Buffer[j + 2] := vec.Z;

            // do include normals?
            if (EQR_VF_Normals in mesh1[i].m_Format) then
            begin
                // get normals
                srcNormal.X := mesh1[i].m_Buffer[j + index];
                srcNormal.Y := mesh1[i].m_Buffer[j + index + 1];
                srcNormal.Z := mesh1[i].m_Buffer[j + index + 2];
                dstNormal.X := mesh2[i].m_Buffer[j + index];
                dstNormal.Y := mesh2[i].m_Buffer[j + index + 1];
                dstNormal.Z := mesh2[i].m_Buffer[j + index + 2];

                // interpolate normals
                normal := srcNormal.Interpolate(dstNormal, position);

                // set interpolated normals in destination buffer
                SetLength(vertex.m_Buffer, Length(vertex.m_Buffer) + 3);
                vertex.m_Buffer[j + index]     := normal.X;
                vertex.m_Buffer[j + index + 1] := normal.Y;
                vertex.m_Buffer[j + index + 2] := normal.Z;
                Inc(index, 3);
            end;

            // do include texture coordinates?
            if (EQR_VF_TexCoords in mesh1[i].m_Format) then
            begin
                // copy texture coordinates from source
                SetLength(vertex.m_Buffer, Length(vertex.m_Buffer) + 2);
                vertex.m_Buffer[j + index]     := mesh1[i].m_Buffer[j + index];
                vertex.m_Buffer[j + index + 1] := mesh1[i].m_Buffer[j + index + 1];
                Inc(index, 2);
            end;

            // do include colors?
            if (EQR_VF_Colors in mesh1[i].m_Format) then
            begin
                // copy color from source
                SetLength(vertex.m_Buffer, Length(vertex.m_Buffer) + 4);
                vertex.m_Buffer[j + index]     := mesh1[i].m_Buffer[j + index];
                vertex.m_Buffer[j + index + 1] := mesh1[i].m_Buffer[j + index + 1];
                vertex.m_Buffer[j + index + 2] := mesh1[i].m_Buffer[j + index + 2];
                vertex.m_Buffer[j + index + 3] := mesh1[i].m_Buffer[j + index + 3];
            end;

            Inc(j, mesh1[i].m_Stride);
        end;

        // add interpolated mesh to output list
        SetLength(mesh, Length(mesh) + 1);
        mesh[Length(mesh) - 1] := vertex;

        // delete vertex
        SetLength(vertex.m_Buffer, 0);
    end;

    Result := True;
end;
//------------------------------------------------------------------------------
// TQRModelCache
//------------------------------------------------------------------------------
constructor TQRModelCache.Create();
begin
    inherited Create;

    m_pMeshCache          := TQRCache<NativeUInt, PQRMesh>.Create;
    m_pAABBTreeCache      := TQRCache<NativeUInt, TQRAABBTree>.Create;

    // set callbacks
    m_pMeshCache.OnDeleteFromCache     := OnDeleteMesh;
    m_pAABBTreeCache.OnDeleteFromCache := OnDeleteAABBTree;
end;
//------------------------------------------------------------------------------
destructor TQRModelCache.Destroy();
begin
    // clear memory
    m_pAABBTreeCache.Free;
    m_pMeshCache.Free;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRModelCache.OnDeleteMesh(const key: NativeUInt; var pMesh: PQRMesh): Boolean;
begin
    if (Assigned(pMesh)) then
        Dispose(pMesh);

    Result := True;
end;
//------------------------------------------------------------------------------
function TQRModelCache.OnDeleteAABBTree(const key: NativeUInt; var pTree: TQRAABBTree): Boolean;
begin
    pTree.Free;
    Result := True;
end;
//------------------------------------------------------------------------------
function TQRModelCache.GetMesh(i: NativeUInt): PQRMesh;
begin
    // get mesh from cache if exists, otherwise returns nil
    if (not m_pMeshCache.Get(i, Result)) then
        Result := nil;
end;
//------------------------------------------------------------------------------
procedure TQRModelCache.SetMesh(i: NativeUInt; pMesh: PQRMesh);
begin
    // cache mesh, clear the previous entry if exists. Be careful, the cache will take the ownership
    // of the received mesh, so don't try to delete it externally
    m_pMeshCache.Add(i, pMesh);
end;
//------------------------------------------------------------------------------
function TQRModelCache.GetTree(i: NativeUInt): TQRAABBTree;
begin
    // get tree from cache if exists, otherwise returns nil
    if (not m_pAABBTreeCache.Get(i, Result)) then
        Result := nil;
end;
//------------------------------------------------------------------------------
procedure TQRModelCache.SetTree(i: NativeUInt; pTree: TQRAABBTree);
begin
    // cache tree, clear the previous entry if exists. Be careful, the cache will take the ownership
    // of the received tree, so don't try to delete it externally
    m_pAABBTreeCache.Add(i, pTree);
end;
//------------------------------------------------------------------------------
// TQRModelParser
//------------------------------------------------------------------------------
constructor TQRModelParser.Create();
begin
    inherited Create;
end;
//------------------------------------------------------------------------------
destructor TQRModelParser.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
// TQRModel
//------------------------------------------------------------------------------
constructor TQRModel.Create();
begin
    inherited Create;

    m_pColor := TQRColor.Create(255, 255, 255, 255);
end;
//------------------------------------------------------------------------------
destructor TQRModel.Destroy();
begin
    m_pColor.Free;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRModel.GetColor(): TQRColor;
begin
    Result := m_pColor;
end;
//------------------------------------------------------------------------------
procedure TQRModel.SetColor(const pColor: TQRColor);
begin
    m_pColor.Assign(pColor);
end;
//------------------------------------------------------------------------------
function TQRModel.GetVertexFormat(): TQRVertexFormat;
begin
    Result := m_VertexFormat;
end;
//------------------------------------------------------------------------------
procedure TQRModel.SetVertexFormat(value: TQRVertexFormat);
begin
    m_VertexFormat := value;
end;
//------------------------------------------------------------------------------
// TQRStaticModel
//------------------------------------------------------------------------------
constructor TQRStaticModel.Create();
begin
    inherited Create;
end;
//------------------------------------------------------------------------------
destructor TQRStaticModel.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
// TQRFramedModel
//------------------------------------------------------------------------------
constructor TQRFramedModel.Create();
begin
    inherited Create;
end;
//------------------------------------------------------------------------------
destructor TQRFramedModel.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
// TQRArticulatedModel
//------------------------------------------------------------------------------
constructor TQRArticulatedModel.Create();
begin
    inherited Create;
end;
//------------------------------------------------------------------------------
destructor TQRArticulatedModel.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------

end.
