{**************************************************************************************************
 * ==> UTQRMD3 -----------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides the tools to load a md3 model and build his vertex buffer   *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRMD3;

interface

uses System.Classes,
     System.SysUtils,
     System.Generics.Collections,
     UTQRCommon,
     UTQRGraphics,
     UTQR3D,
     UTQRGeometry,
     UTQRCollision,
     UTQRModel;

const
    //----------------------------------------------------------------------------------------------
    // Global constants
    //----------------------------------------------------------------------------------------------
    CQR_MD3_MAX_QPATH                     = 63;
    CQR_MD3_Mesh_File_Version: TQRUInt32  = 15;
    CQR_MD3_ID:                TQRUInt32  = ($33 shl 24) + ($50 shl 16) + ($44 shl 8) + $49;
    CQR_MD3_XYZ_Scale:         TQRFloat32 = 64.0; // as shown in doc http://www.icculus.org/~phaethon/q3a/formats/md3format.html
    CQR_MD3_Invalid_Index:     NativeUInt = NativeUInt(-1);
    //----------------------------------------------------------------------------------------------

type
    {**
    * MD3 header
    *}
    TQRMD3Header = record
        m_ID:             TQRUInt32;
        m_Version:        TQRUInt32;
        m_FileName:       array [0..CQR_MD3_MAX_QPATH] of AnsiChar;
        m_Flags:          TQRUInt32;
        m_FrameCount:     TQRUInt32;
        m_TagCount:       TQRUInt32;
        m_MeshCount:      TQRUInt32;
        m_SkinCount:      TQRUInt32;
        m_FrameOffset:    TQRUInt32;
        m_TagStartOffset: TQRUInt32;
        m_TagEndOffset:   TQRUInt32;
        m_FileSize:       TQRUInt32;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Header = ^TQRMD3Header;

    {**
    * MD3 mesh info
    *}
    TQRMD3MeshInfo = record
        m_MeshID:         TQRUInt32;
        m_Name:           array [0..CQR_MD3_MAX_QPATH] of AnsiChar;
        m_Flags:          TQRUInt32;
        m_MeshFrameCount: TQRUInt32;
        m_SkinCount:      TQRUInt32;
        m_PolygonCount:   TQRUInt32;
        m_FaceCount:      TQRUInt32;
        m_FaceOffset:     TQRUInt32;
        m_SkinOffset:     TQRUInt32;
        m_UVOffset:       TQRUInt32;
        m_PolygonOffset:  TQRUInt32;
        m_MeshSize:       TQRUInt32;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3MeshInfo = ^TQRMD3MeshInfo;

    {**
    * MD3 tag structure, used to link, rotate and translate the children models
    *}
    TQRMD3Tag = record
        m_Name:     array [0..CQR_MD3_MAX_QPATH] of AnsiChar;
        m_Position: array [0..2] of TQRFloat32;
        m_Rotation: array [0..2] of array [0..2] of TQRFloat32;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Tag = ^TQRMD3Tag;

    {**
    * MD3 bone info
    *}
    TQRMD3Bone = record
        m_Min:      array [0..2] of TQRFloat32;
        m_Max:      array [0..2] of TQRFloat32;
        m_Position: array [0..2] of TQRFloat32;
        m_Scale:    TQRFloat32;
        m_Creator:  array [0..15] of AnsiChar;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Bone = ^TQRMD3Bone;

    {**
    * MD3 polygon (also named vertex in documentation), contains the normals and vertex indices
    *}
    TQRMD3Polygon = record
        m_Vertex: array [0..2] of TQRInt16;
        m_Normal: array [0..1] of TQRUInt8;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Polygon = ^TQRMD3Polygon;

    {**
    * MD3 face (also named triangle in documentation), contains the vertex indices and texture
    * coordinate arrays
    *@note This class cannot be inherited
    *}
    TQRMD3Face = record
        m_VertexIndices: array [0..2] of TQRUInt32;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Face = ^TQRMD3Face;

    {**
    * MD3 texture UV coordinates
    *}
    TQRMD3TextureCoord = record
        m_Coordinate: array [0..1] of TQRFloat32;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3TextureCoord = ^TQRMD3TextureCoord;

    {**
    * MD3 skin (i.e. texture name, also name shader in documentation)
    *}
    TQRMD3Skin = record
        m_Name:        array [0..CQR_MD3_MAX_QPATH] of AnsiChar;
        m_ShaderIndex: TQRUInt32;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Skin = ^TQRMD3Skin;

    {**
    * MD3 mesh
    *}
    TQRMD3Mesh = record
        m_Info:     TQRMD3MeshInfo;
        m_Skin:     array of TQRMD3Skin;
        m_TexCoord: array of TQRMD3TextureCoord;
        m_Face:     array of TQRMD3Face;
        m_Polygon:  array of TQRMD3Polygon;
    end;

    PQRMD3Mesh = ^TQRMD3Mesh;

    {**
    * Reads and exposes MD3 file content
    *@note This class is cross-platform
    *@author Jean-Milost Reymond
    *}
    TQRMD3Parser = class(TQRModelParser)
        protected
            m_Header: TQRMD3Header;
            m_Bone:   array of TQRMD3Bone;
            m_Tag:    array of TQRMD3Tag;
            m_Mesh:   array of TQRMD3Mesh;

            {**
            * Gets header
            *@return header, nil if not found or on error
            *}
            function GetHeader(): PQRMD3Header; virtual;

            {**
            * Gets bone at index
            *@param index - index
            *@return bone, nil if not found or on error
            *}
            function GetBone(index: NativeInt): PQRMD3Bone; virtual;

            {**
            * Gets tag at index
            *@param index - index
            *@return tag, nil if not found or on error
            *}
            function GetTag(index: NativeInt): PQRMD3Tag; virtual;

            {**
            * Gets mesh at index
            *@param index - index
            *@return mesh, nil if not found or on error
            *}
            function GetMesh(index: NativeInt): PQRMD3Mesh; virtual;

            {**
            * Gets bone count
            *@return bone count
            *}
            function GetBoneCount(): NativeInt; virtual;

            {**
            * Gets tag count
            *@return tag count
            *}
            function GetTagCount(): NativeInt; virtual;

            {**
            * Gets mesh count
            *@return mesh count
            *}
            function GetMeshCount(): NativeInt; virtual;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads MD3 from file
            *@param fileName - file name
            *@return true on success, otherwise false
            *}
            function Load(const fileName: TFileName): Boolean; override;

            {**
            * Loads MD3 from buffer
            *@param pBuffer - buffer
            *@param readLength - length to read in buffer, in bytes (not used here, can be 0)
            *@return true on success, otherwise false
            *@note Read will begin from current offset
            *}
            function Load(const pBuffer: TStream; readLength: NativeUInt): Boolean; override;

            { Properties }
            property Header:                   PQRMD3Header read GetHeader;
            property Bones [index: NativeInt]: PQRMD3Bone   read GetBone;
            property Tags  [index: NativeInt]: PQRMD3Tag    read GetTag;
            property Meshes[index: NativeInt]: PQRMD3Mesh   read GetMesh;
            property BoneCount:                NativeInt    read GetBoneCount;
            property TagCount:                 NativeInt    read GetTagCount;
            property MeshCount:                NativeInt    read GetMeshCount;
    end;

    TQRMD3Vertices           = array of TQRVector3D;
    TQRMD3Normals            = array of TQRVector3D;
    TQRMD3TexCoords          = array of TQRTexCoord;
    TQRMD3VerticesDictionary = TDictionary<NativeUInt, TQRMD3Vertices>;
    TQRMD3NormalDictionary   = TDictionary<NativeUInt, TQRMD3Normals>;
    TQRMD3TextureDictionary  = TDictionary<NativeUInt, TQRMD3TexCoords>;

    {**
    * MD3 model
    *}
    TQRMD3Model = class(TQRFramedModel)
        protected
            m_pParser:    TQRMD3Parser;
            m_pVertices:  TQRMD3VerticesDictionary;
            m_pNormals:   TQRMD3NormalDictionary;
            m_pTexCoords: TQRMD3TextureDictionary;

            {**
            * Prepares mesh to be used by mesh generator
            *}
            procedure PrepareMesh; virtual;

            {**
            * Adds vertex to vertex buffer
            *@param index - mesh index to get
            *@param indice - vertex indice as read from md3 file
            *@param vertexFormat - vertex format to apply
            *@param vertices - vertices list read from md3 file
            *@param normals - normals list read from md3 file
            *@param texCoords - texture coordinate list read from md3 file
            *@param color - vertex color to apply
            *@param vertex - vertex containing buffer to add to
            *}
            procedure AddVertex(index, indice: NativeInt;
                                 vertexFormat: TQRVertexFormat;
                               const vertices: TQRMD3Vertices;
                                const normals: TQRMD3Normals;
                              const texCoords: TQRMD3TexCoords;
                                 const pColor: TQRColor;
                                   var vertex: TQRVertex); overload; virtual;

            {**
            * Adds vertex to vertex buffer, interpolate current and next vertex
            *@param index - mesh index to get
            *@param nextIndex - next mesh index to interpolate with
            *@param indice - vertex indice as read from md3 file
            *@aram interpolationFactor - interpolation factor to apply
            *@param vertexFormat - vertex format to apply
            *@param vertices - vertices list read from md3 file
            *@param normals - normals list read from md3 file
            *@param texCoords - texture coordinate list read from md3 file
            *@param color - vertex color to apply
            *@param vertex - vertex containing buffer to add to
            *}
            procedure AddVertex(index, nextIndex, indice: NativeInt;
                                     interpolationFactor: Single;
                                            vertexFormat: TQRVertexFormat;
                                          const vertices: TQRMD3Vertices;
                                           const normals: TQRMD3Normals;
                                         const texCoords: TQRMD3TexCoords;
                                            const pColor: TQRColor;
                                              var vertex: TQRVertex); overload; virtual;

            {**
            * Uncompresses normal
            *@param latitude - normal latitude as written if md3 file
            *@param longitude - normal longitude as written if md3 file
            *@return uncompressed normal
            *}
            function UncompressNormal(latitude, longitude: Byte): TQRVector3D; virtual;

            {**
            * Gets MD3 parser
            *@return MD3 parser
            *}
            function GetParser(): TQRMD3Parser; virtual;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads MD3 from file
            *@param fileName - file name
            *@return true on success, otherwise false
            *}
            function Load(const fileName: TFileName): Boolean; overload; virtual;

            {**
            * Loads MD3 from buffer
            *@param pBuffer - buffer
            *@param readLength - length to read in buffer, in bytes
            *@return true on success, otherwise false
            *@note Read will begin from current offset
            *}
            function Load(const pBuffer: TStream;
                             readLength: NativeUInt): Boolean; overload; virtual;

            {**
            * Gets model frame mesh
            *@param index - frame mesh index to create
            *@param[out] mesh - frame mesh
            *@param pAABBTree - aligned-axis bounding box tree to populate, ignored if nil
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *@note vertex buffer content is organized as follow:
            *      [1]x [2]y [3]z [4]nx [5]ny [6]nz [7]tu [8]tv [9]r [10]g [11]b [12]a
            *      where:
            *      x/y/z    - vertex coordinates
            *      nx/ny/nz - vertex normal (if includeNormal is activated)
            *      tu/tv    - vertex texture coordinates(if includeTexture is activated)
            *      r/g/b/a  - vertex color(if includeColor is activated)
            *}
            function GetMesh(index: NativeUInt;
                          out mesh: TQRMesh;
                         pAABBTree: TQRAABBTree;
                       hIsCanceled: TQRIsCanceledEvent = nil): Boolean; overload; override;

            {**
            * Gets model frame mesh
            *@param index - frame mesh index to get
            *@param nextIndex - frame mesh index to interpolate with
            *@param interpolationFactor - interpolation factor to apply
            *@param[out] mesh - frame mesh
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *@note vertex buffer content is organized as follow:
            *      [1]x [2]y [3]z [4]nx [5]ny [6]nz [7]tu [8]tv [9]r [10]g [11]b [12]a
            *      where:
            *      x/y/z    - vertex coordinates
            *      nx/ny/nz - vertex normal (if includeNormal is activated)
            *      tu/tv    - vertex texture coordinates(if includeTexture is activated)
            *      r/g/b/a  - vertex color(if includeColor is activated)
            *}
            function GetMesh(index, nextIndex: NativeUInt;
                          interpolationFactor: Double;
                                     out mesh: TQRMesh;
                                  hIsCanceled: TQRIsCanceledEvent): Boolean; overload; override;

            {**
            * Gets mesh count
            *@return mesh count
            *}
            function GetMeshCount(): NativeUInt; override;

            { Properties }
            property Parser: TQRMD3Parser read GetParser;
    end;

implementation
//------------------------------------------------------------------------------
// TQRMD3Header
//------------------------------------------------------------------------------
procedure TQRMD3Header.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Header), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(PQRUInt32(m_ID),             SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_Version),        SizeOf(TQRUInt32));
    pBuffer.Read(m_FileName,                  SizeOf(m_FileName));
    pBuffer.Read(PQRUInt32(m_Flags),          SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FrameCount),     SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_TagCount),       SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_MeshCount),      SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_SkinCount),      SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FrameOffset),    SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_TagStartOffset), SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_TagEndOffset),   SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FileSize),       SizeOf(TQRUInt32));
end;
//------------------------------------------------------------------------------
// TQRMD3MeshInfo
//------------------------------------------------------------------------------
procedure TQRMD3MeshInfo.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3MeshInfo), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(PQRUInt32(m_MeshID),         SizeOf(TQRUInt32));
    pBuffer.Read(m_Name,                      SizeOf(m_Name));
    pBuffer.Read(PQRUInt32(m_Flags),          SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_MeshFrameCount), SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_SkinCount),      SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_PolygonCount),   SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FaceCount),      SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FaceOffset),     SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_SkinOffset),     SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_UVOffset),       SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_PolygonOffset),  SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_MeshSize),       SizeOf(TQRUInt32));
end;
//------------------------------------------------------------------------------
// TQRMD3Tag
//------------------------------------------------------------------------------
procedure TQRMD3Tag.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Tag), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_Name,     SizeOf(m_Name));
    pBuffer.Read(m_Position, SizeOf(m_Position));
    pBuffer.Read(m_Rotation, SizeOf(m_Rotation));
end;
//------------------------------------------------------------------------------
// TQRMD3Bone
//------------------------------------------------------------------------------
procedure TQRMD3Bone.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Bone), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_Min,               SizeOf(m_Min));
    pBuffer.Read(m_Max,               SizeOf(m_Max));
    pBuffer.Read(m_Position,          SizeOf(m_Position));
    pBuffer.Read(PQRFloat32(m_Scale), SizeOf(TQRFloat32));
    pBuffer.Read(m_Creator,           SizeOf(m_Creator));
end;
//------------------------------------------------------------------------------
// TQRMD3Polygon
//------------------------------------------------------------------------------
procedure TQRMD3Polygon.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Polygon), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_Vertex, SizeOf(m_Vertex));
    pBuffer.Read(m_Normal, SizeOf(m_Normal));
end;
//------------------------------------------------------------------------------
// TQRMD3Face
//------------------------------------------------------------------------------
procedure TQRMD3Face.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Face), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_VertexIndices, SizeOf(m_VertexIndices));
end;
//------------------------------------------------------------------------------
// TQRMD3TextureCoord
//------------------------------------------------------------------------------
procedure TQRMD3TextureCoord.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3TextureCoord), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_Coordinate, SizeOf(m_Coordinate));
end;
//------------------------------------------------------------------------------
// TQRMD3Skin
//------------------------------------------------------------------------------
procedure TQRMD3Skin.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Skin), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_Name,                   SizeOf(m_Name));
    pBuffer.Read(pQRUInt32(m_ShaderIndex), SizeOf(TQRUInt32));
end;
//------------------------------------------------------------------------------
// TQRMD3Parser
//------------------------------------------------------------------------------
constructor TQRMD3Parser.Create();
begin
    inherited Create;
end;
//------------------------------------------------------------------------------
destructor TQRMD3Parser.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRMD3Parser.GetHeader(): PQRMD3Header;
begin
    Result := @m_Header;
end;
//------------------------------------------------------------------------------
function TQRMD3Parser.GetBone(index: NativeInt): PQRMD3Bone;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Bone))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Bone[index];
end;
//------------------------------------------------------------------------------
function TQRMD3Parser.GetTag(index: NativeInt): PQRMD3Tag;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Tag))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Tag[index];
end;
//------------------------------------------------------------------------------
function TQRMD3Parser.GetMesh(index: NativeInt): PQRMD3Mesh;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Mesh))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Mesh[index];
end;
//------------------------------------------------------------------------------
function TQRMD3Parser.GetBoneCount(): NativeInt;
begin
    Result := Length(m_Bone);
end;
//------------------------------------------------------------------------------
function TQRMD3Parser.GetTagCount(): NativeInt;
begin
    Result := Length(m_Tag);
end;
//------------------------------------------------------------------------------
function TQRMD3Parser.GetMeshCount(): NativeInt;
begin
    Result := Length(m_Mesh);
end;
//------------------------------------------------------------------------------
function TQRMD3Parser.Load(const fileName: TFileName): Boolean;
var
    pBuffer: TFileStream;
begin
    pBuffer := nil;

    try
        // file exists?
        if (not FileExists(fileName)) then
        begin
            Result := False;
            Exit;
        end;

        // create a file buffer and open it for read
        pBuffer := TFileStream.Create(fileName, fmOpenRead);
        pBuffer.Seek(0, soBeginning);

        // read MD3 content
        Result := Load(pBuffer, pBuffer.Size);
    finally
        // clear buffer
        pBuffer.Free;
    end;
end;
//------------------------------------------------------------------------------
function TQRMD3Parser.Load(const pBuffer: TStream; readLength: NativeUInt): Boolean;
var
    i, j, tagCount, polygonFrameCount: Cardinal;
    offset:                            NativeUInt;
begin
    try
        // is pBuffer empty?
        if (pBuffer.Size = 0) then
        begin
            Result := False;
            Exit;
        end;

        // read file header
        m_Header.Read(pBuffer);

        // is MD3 file and version correct?
        if ((m_Header.m_ID <> CQR_MD3_ID) or (m_Header.m_Version <> CQR_MD3_Mesh_File_Version)) then
        begin
            Result := False;
            Exit;
        end;

        // create bones
        SetLength(m_Bone, m_Header.m_FrameCount);

        // read bones
        if (m_Header.m_FrameCount > 0) then
            for i := 0 to m_Header.m_FrameCount - 1 do
                m_Bone[i].Read(pBuffer);

        // get tag count
        tagCount := m_Header.m_FrameCount * m_Header.m_TagCount;

        // create tags, for each animation there is a tag array
        SetLength(m_Tag, tagCount);

        // read tags
        if (tagCount > 0) then
            for i := 0 to tagCount - 1 do
                m_Tag[i].Read(pBuffer);

        // create meshes
        SetLength(m_Mesh, m_Header.m_MeshCount);

        // get current offset
        offset := pBuffer.Position;

        // read meshes
        if (m_Header.m_MeshCount > 0) then
            // iterate through meshes to read
            for i := 0 to m_Header.m_MeshCount - 1 do
            begin
                // go to next mesh
                pBuffer.Seek(offset, soBeginning);

                // read mesh header
                m_Mesh[i].m_Info.Read(pBuffer);

                // get polygon count
                polygonFrameCount :=
                        m_Mesh[i].m_Info.m_MeshFrameCount * m_Mesh[i].m_Info.m_PolygonCount;

                // create mesh structure
                SetLength(m_Mesh[i].m_Skin,     m_Mesh[i].m_Info.m_SkinCount);
                SetLength(m_Mesh[i].m_Face,     m_Mesh[i].m_Info.m_FaceCount);
                SetLength(m_Mesh[i].m_TexCoord, m_Mesh[i].m_Info.m_PolygonCount);
                SetLength(m_Mesh[i].m_Polygon,  polygonFrameCount);

                // read skins
                if (m_Mesh[i].m_Info.m_SkinCount > 0) then
                    for j := 0 to m_Mesh[i].m_Info.m_SkinCount - 1 do
                        m_Mesh[i].m_Skin[j].Read(pBuffer);

                // go to faces offset
                pBuffer.Seek(offset + m_Mesh[i].m_Info.m_FaceOffset, soBeginning);

                // read faces (also named triangles in many documentation)
                if (m_Mesh[i].m_Info.m_FaceCount > 0) then
                    for j := 0 to m_Mesh[i].m_Info.m_FaceCount - 1 do
                        m_Mesh[i].m_Face[j].Read(pBuffer);

                // go to texture coords offset
                pBuffer.Seek(offset + m_Mesh[i].m_Info.m_UVOffset, soBeginning);

                // read texture coords
                if (m_Mesh[i].m_Info.m_PolygonCount > 0) then
                    for j := 0 to m_Mesh[i].m_Info.m_PolygonCount - 1 do
                        m_Mesh[i].m_TexCoord[j].Read(pBuffer);

                // go to polygons offset
                pBuffer.Seek(offset + m_Mesh[i].m_Info.m_PolygonOffset, soBeginning);

                // read polygons (also named vertices in many documentation)
                if (polygonFrameCount > 0) then
                    for j := 0 to polygonFrameCount - 1 do
                        m_Mesh[i].m_Polygon[j].Read(pBuffer);

                // calculate next mesh offset
                Inc(offset, m_Mesh[i].m_Info.m_MeshSize);
            end;

        Result := True;
        Exit;
    except
        on e: Exception do ; // ignore any error
    end;

    Result := False;
end;
//------------------------------------------------------------------------------
// TQRMD3Model
//------------------------------------------------------------------------------
constructor TQRMD3Model.Create();
begin
    inherited Create;

    m_pParser    := TQRMD3Parser.Create();
    m_pVertices  := TQRMD3VerticesDictionary.Create();
    m_pNormals   := TQRMD3NormalDictionary.Create();
    m_pTexCoords := TQRMD3TextureDictionary.Create();
end;
//------------------------------------------------------------------------------
destructor TQRMD3Model.Destroy();
begin
    // clear memory
    m_pParser.Free;
    m_pVertices.Free;
    m_pNormals.Free;
    m_pTexCoords.Free;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRMD3Model.PrepareMesh;
var
    vertexCount, texCoordCount, i, j: NativeUInt;
    vertices:                         TQRMD3Vertices;
    normals:                          TQRMD3Normals;
    texCoords:                        TQRMD3TexCoords;
begin
    m_pVertices.Clear;
    m_pNormals.Clear;
    m_pTexCoords.Clear;

    // no meshes to get?
    if (m_pParser.m_Header.m_MeshCount = 0) then
        Exit;

    // iterate through meshes to get
    for i := 0 to m_pParser.m_Header.m_MeshCount - 1 do
    begin
        // get mesh vertex count
        vertexCount :=
                m_pParser.m_Mesh[i].m_Info.m_PolygonCount * m_pParser.m_Mesh[i].m_Info.m_MeshFrameCount;

        if (vertexCount = 0) then
            continue;

        try
            // reserve memory for temporary vertices to get from md3 file
            SetLength(vertices, vertexCount);

            // reserve memory for temporary normals to get from md3 file
            SetLength(normals, vertexCount);

            // iterate through vertices to get
            for j := 0 to vertexCount - 1 do
            begin
                // uncompress vertex and add it to temporary vertex list
                vertices[j].X := m_pParser.m_Mesh[i].m_Polygon[j].m_Vertex[0] / CQR_MD3_XYZ_Scale;
                vertices[j].Y := m_pParser.m_Mesh[i].m_Polygon[j].m_Vertex[1] / CQR_MD3_XYZ_Scale;
                vertices[j].Z := m_pParser.m_Mesh[i].m_Polygon[j].m_Vertex[2] / CQR_MD3_XYZ_Scale;

                // uncompress normal and add it to temporary normal list
                normals[j] := UncompressNormal(m_pParser.m_Mesh[i].m_Polygon[j].m_Normal[0],
                                               m_pParser.m_Mesh[i].m_Polygon[j].m_Normal[1]);
            end;

            m_pVertices.Add(i, vertices);
            m_pNormals.Add(i, normals);
        finally
            SetLength(vertices, 0);
            SetLength(normals, 0);
        end;

        // get texture coordinates count
        texCoordCount := m_pParser.m_Mesh[i].m_Info.m_PolygonCount;

        try
            // reserve memory for temporary texture coordinates to get from md3 file
            SetLength(texCoords, texCoordCount);

            // iterate through texture coordinates to get
            for j := 0 to texCoordCount - 1 do
            begin
                // uncompress texture coordinates and add them to temporary texure coord list
                texCoords[j].U := m_pParser.m_Mesh[i].m_TexCoord[j].m_Coordinate[0];
                texCoords[j].V := m_pParser.m_Mesh[i].m_TexCoord[j].m_Coordinate[1];
            end;

            m_pTexCoords.Add(i, texCoords);
        finally
            SetLength(texCoords, 0);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure TQRMD3Model.AddVertex(index, indice: NativeInt;
                                 vertexFormat: TQRVertexFormat;
                               const vertices: TQRMD3Vertices;
                                const normals: TQRMD3Normals;
                              const texCoords: TQRMD3TexCoords;
                                 const pColor: TQRColor;
                                   var vertex: TQRVertex);
var
    frameIndex: NativeInt;
    offset:     NativeUInt;
begin
    // calculate frame index
    frameIndex := index + indice;

    // is frame index out of bounds?
    if (frameIndex >= Length(vertices)) then
        raise Exception.Create('Vertice indice is out of bounds');

    // get current vertex offset
    offset := Length(vertex.m_Buffer);

    // add memory for vertex
    SetLength(vertex.m_Buffer, offset + 3);

    // copy vertex
    vertex.m_Buffer[offset]     := vertices[frameIndex].X;
    vertex.m_Buffer[offset + 1] := vertices[frameIndex].Y;
    vertex.m_Buffer[offset + 2] := vertices[frameIndex].Z;

    // increment offset
    Inc(offset, 3);

    // do include normals?
    if (EQR_VF_Normals in m_VertexFormat) then
    begin
        // is indice out of bounds?
        if (frameIndex >= Length(normals)) then
            raise Exception.Create('Vertice indice is out of bounds');

        // add memory for normal
        SetLength(vertex.m_Buffer, offset + 3);

        // copy normal
        vertex.m_Buffer[offset]     := normals[frameIndex].X;
        vertex.m_Buffer[offset + 1] := normals[frameIndex].Y;
        vertex.m_Buffer[offset + 2] := normals[frameIndex].Z;

        // increment offset
        Inc(offset, 3);
    end;

    // do include texture coordinates?
    if (EQR_VF_TexCoords in m_VertexFormat) then
    begin
        // is indice out of bounds?
        if (indice >= Length(texCoords)) then
            raise Exception.Create('Vertice indice is out of bounds');

        // add memory for texture coordinates
        SetLength(vertex.m_Buffer, offset + 2);

        vertex.m_Buffer[offset]     := texCoords[indice].U;
        vertex.m_Buffer[offset + 1] := texCoords[indice].V;

        // increment offset
        Inc(offset, 2);
    end;

    // do include colors?
    if (EQR_VF_Colors in m_VertexFormat) then
    begin
        // add memory for texture coordinates
        SetLength(vertex.m_Buffer, offset + 4);

        vertex.m_Buffer[offset]     := color.GetRedF();
        vertex.m_Buffer[offset + 1] := color.GetGreenF();
        vertex.m_Buffer[offset + 2] := color.GetBlueF();
        vertex.m_Buffer[offset + 3] := color.GetAlphaF();
    end;
end;
//------------------------------------------------------------------------------
procedure TQRMD3Model.AddVertex(index, nextIndex, indice: NativeInt;
                                     interpolationFactor: Single;
                                            vertexFormat: TQRVertexFormat;
                                          const vertices: TQRMD3Vertices;
                                           const normals: TQRMD3Normals;
                                         const texCoords: TQRMD3TexCoords;
                                            const pColor: TQRColor;
                                              var vertex: TQRVertex);
var
    frameIndex, nextFrameIndex: NativeInt;
    offset:                     NativeUInt;
    finalVertex:                TQRVector3D;
    finalNormal:                TQRVector3D;
begin
    // calculate frame indexes
    frameIndex     := index     + indice;
    nextFrameIndex := nextIndex + indice;

    // is frame index out of bounds?
    if ((frameIndex >= Length(vertices)) or (nextFrameIndex >= Length(vertices))) then
        raise Exception.Create('Vertice indice is out of bounds');

    // get current vertex offset
    offset := Length(vertex.m_Buffer);

    // add memory for vertex
    SetLength(vertex.m_Buffer, offset + 3);

    // calculate final mesh vertex
    finalVertex := vertices[frameIndex].Interpolate(vertices[nextFrameIndex], interpolationFactor);

    // copy vertex
    vertex.m_Buffer[offset]     := finalVertex.X;
    vertex.m_Buffer[offset + 1] := finalVertex.Y;
    vertex.m_Buffer[offset + 2] := finalVertex.Z;

    // increment offset
    Inc(offset, 3);

    // do include normals?
    if (EQR_VF_Normals in m_VertexFormat) then
    begin
        // is indice out of bounds?
        if ((frameIndex >= Length(normals))  or (nextFrameIndex >= Length(normals))) then
            raise Exception.Create('Vertice indice is out of bounds');

        // add memory for normal
        SetLength(vertex.m_Buffer, offset + 3);

        // calculate final mesh normal
        finalNormal := normals[frameIndex].Interpolate(normals[nextFrameIndex], interpolationFactor);

        // copy normal
        vertex.m_Buffer[offset]     := finalNormal.X;
        vertex.m_Buffer[offset + 1] := finalNormal.Y;
        vertex.m_Buffer[offset + 2] := finalNormal.Z;

        // increment offset
        Inc(offset, 3);
    end;

    // do include texture coordinates?
    if (EQR_VF_TexCoords in m_VertexFormat) then
    begin
        // is indice out of bounds?
        if (indice >= Length(texCoords)) then
            raise Exception.Create('Vertice indice is out of bounds');

        // add memory for texture coordinates
        SetLength(vertex.m_Buffer, offset + 2);

        vertex.m_Buffer[offset]     := texCoords[indice].U;
        vertex.m_Buffer[offset + 1] := texCoords[indice].V;

        // increment offset
        Inc(offset, 2);
    end;

    // do include colors?
    if (EQR_VF_Colors in m_VertexFormat) then
    begin
        vertex.m_Buffer[offset]     := color.GetRedF();
        vertex.m_Buffer[offset + 1] := color.GetGreenF();
        vertex.m_Buffer[offset + 2] := color.GetBlueF();
        vertex.m_Buffer[offset + 3] := color.GetAlphaF();
    end;
end;
//------------------------------------------------------------------------------
function TQRMD3Model.UncompressNormal(latitude, longitude: Byte): TQRVector3D;
var
    ratio, lng, lat: Single;
begin
    // calculate uncompression ratio
    ratio := (PI / 128.0);

    // apply ratio to latitude and longitude
    lat := latitude  * ratio;
    lng := longitude * ratio;

    // calculate and return normal
    Result := TQRVector3D.Create(Cos(lat) * Sin(lng), Sin(lat) * Sin(lng), Cos(lng));
end;
//------------------------------------------------------------------------------
function TQRMD3Model.GetParser(): TQRMD3Parser;
begin
    Result := m_pParser;
end;
//------------------------------------------------------------------------------
function TQRMD3Model.Load(const fileName: TFileName): Boolean;
begin
    Result := m_pParser.Load(fileName);

    if (Result) then
        PrepareMesh();
end;
//------------------------------------------------------------------------------
function TQRMD3Model.Load(const pBuffer: TStream; readLength: NativeUInt): Boolean;
begin
    Result := m_pParser.Load(pBuffer, readLength);

    if (Result) then
        PrepareMesh();
end;
//------------------------------------------------------------------------------
function TQRMD3Model.GetMesh(index: NativeUInt;
                          out mesh: TQRMesh;
                         pAABBTree: TQRAABBTree;
                       hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    stride, meshIndex, i, j, indiceCount: NativeUInt;
    k:                                    Byte;
begin
    // no mesh count?
    if (m_pParser.m_Header.m_MeshCount = 0) then
    begin
        Result := False;
        Exit;
    end;

    // is frame index out of bounds?
    if (index >= GetMeshCount()) then
    begin
        Result := False;
        Exit;
    end;

    // basically stride is the coordinates values size
    stride := 3;

    // do include normals?
    if (EQR_VF_Normals in m_VertexFormat) then
        Inc(stride, 3);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in m_VertexFormat) then
        Inc(stride, 2);

    // do include colors?
    if (EQR_VF_Colors in m_VertexFormat) then
        Inc(stride, 4);

    // iterate through meshes to get
    for i := 0 to m_pParser.m_Header.m_MeshCount - 1 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
        begin
            Result := False;
            Exit;
        end;

        // check if cache contains vertices to get
        if (not m_pVertices.ContainsKey(i)) then
        begin
            Result := False;
            Exit;
        end;

        // no vertex?
        if (Length(m_pVertices.Items[i]) = 0) then
        begin
            Result := False;
            Exit;
        end;

        // check if cache contains normals to get
        if (not m_pNormals.ContainsKey(i)) then
        begin
            Result := False;
            Exit;
        end;

        // no normals?
        if (Length(m_pNormals.Items[i]) = 0) then
        begin
            Result := False;
            Exit;
        end;

        // check if cache contains textures coordinates to get
        if (not m_pTexCoords.ContainsKey(i)) then
        begin
            Result := False;
            Exit;
        end;

        // no normals?
        if (Length(m_pTexCoords.Items[i]) = 0) then
        begin
            Result := False;
            Exit;
        end;

        // add mesh to output
        meshIndex := Length(mesh);
        SetLength(mesh, meshIndex + 1);

        mesh[meshIndex].m_Name      := UnicodeString(AnsiString(m_pParser.m_Mesh[i].m_Info.m_Name));
        mesh[meshIndex].m_Stride    := stride;
        mesh[meshIndex].m_Type      := EQR_VT_Triangles;
        mesh[meshIndex].m_Format    := m_VertexFormat;
        mesh[meshIndex].m_CoordType := EQR_VC_XYZ;

        // get indice count
        indiceCount := m_pParser.m_Mesh[i].m_Info.m_FaceCount;

        // no indices?
        if (indiceCount = 0) then
        begin
            Result := False;
            Exit;
        end;

        // add memory for vertex buffer
        SetLength(mesh[meshIndex].m_Buffer, indiceCount * 3 * mesh[meshIndex].m_Stride);

        // iterate through vertex indices and create final vertex buffer
        for j := 0 to indiceCount - 1 do
            for k := 0 to 2 do
                AddVertex((index * m_pParser.m_Mesh[i].m_Info.m_PolygonCount),
                          m_pParser.m_Mesh[i].m_Face[j].m_VertexIndices[k],
                          m_VertexFormat,
                          m_pVertices.Items[i],
                          m_pNormals.Items[i],
                          m_pTexCoords.Items[i],
                          m_pColor,
                          mesh[meshIndex]);
    end;

    // populate aligned-axis bounding box tree
    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//------------------------------------------------------------------------------
function TQRMD3Model.GetMesh(index, nextIndex: NativeUInt;
              interpolationFactor: Double;
                         out mesh: TQRMesh;
                      hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    stride, meshIndex, i, j, indiceCount: NativeUInt;
    k:                                    Byte;
begin
    // no mesh count?
    if (m_pParser.m_Header.m_MeshCount = 0) then
    begin
        Result := False;
        Exit;
    end;

    // is frame index out of bounds?
    if (index >= GetMeshCount()) then
    begin
        Result := False;
        Exit;
    end;

    // basically stride is the coordinates values size
    stride := 3;

    // do include normals?
    if (EQR_VF_Normals in m_VertexFormat) then
        Inc(stride, 3);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in m_VertexFormat) then
        Inc(stride, 2);

    // do include colors?
    if (EQR_VF_Colors in m_VertexFormat) then
        Inc(stride, 4);

    // iterate through meshes to get
    for i := 0 to m_pParser.m_Header.m_MeshCount - 1 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
        begin
            Result := False;
            Exit;
        end;

        // check if cache contains vertices to get
        if (not m_pVertices.ContainsKey(i)) then
        begin
            Result := False;
            Exit;
        end;

        // no vertex?
        if (Length(m_pVertices.Items[i]) = 0) then
        begin
            Result := False;
            Exit;
        end;

        // check if cache contains normals to get
        if (not m_pNormals.ContainsKey(i)) then
        begin
            Result := False;
            Exit;
        end;

        // no normals?
        if (Length(m_pNormals.Items[i]) = 0) then
        begin
            Result := False;
            Exit;
        end;

        // check if cache contains textures coordinates to get
        if (not m_pTexCoords.ContainsKey(i)) then
        begin
            Result := False;
            Exit;
        end;

        // no normals?
        if (Length(m_pTexCoords.Items[i]) = 0) then
        begin
            Result := False;
            Exit;
        end;

        // add mesh to output
        meshIndex := Length(mesh);
        SetLength(mesh, meshIndex + 1);

        mesh[meshIndex].m_Name      := UnicodeString(AnsiString(m_pParser.m_Mesh[i].m_Info.m_Name));
        mesh[meshIndex].m_Stride    := stride;
        mesh[meshIndex].m_Type      := EQR_VT_Triangles;
        mesh[meshIndex].m_Format    := m_VertexFormat;
        mesh[meshIndex].m_CoordType := EQR_VC_XYZ;

        // get indice count
        indiceCount := m_pParser.m_Mesh[i].m_Info.m_FaceCount;

        // no indices?
        if (indiceCount = 0) then
        begin
            Result := False;
            Exit;
        end;

        // add memory for vertex buffer
        SetLength(mesh[meshIndex].m_Buffer, indiceCount * 3 * mesh[meshIndex].m_Stride);

        // iterate through vertex indices and create final vertex buffer
        for j := 0 to indiceCount - 1 do
            for k := 0 to 2 do
                AddVertex((index     * m_pParser.m_Mesh[i].m_Info.m_PolygonCount),
                          (nextIndex * m_pParser.m_Mesh[i].m_Info.m_PolygonCount),
                          m_pParser.m_Mesh[i].m_Face[j].m_VertexIndices[k],
                          interpolationFactor,
                          m_VertexFormat,
                          m_pVertices.Items[i],
                          m_pNormals.Items[i],
                          m_pTexCoords.Items[i],
                          m_pColor,
                          mesh[meshIndex]);
    end;

    Result := True;
end;
//------------------------------------------------------------------------------
function TQRMD3Model.GetMeshCount(): NativeUInt;
begin
    Result := m_pParser.m_Header.m_FrameCount;
end;
//------------------------------------------------------------------------------

end.
