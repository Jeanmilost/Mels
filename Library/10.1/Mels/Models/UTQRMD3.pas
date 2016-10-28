// *************************************************************************************************
// * ==> UTQRMD3 ----------------------------------------------------------------------------------*
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
 @abstract(@name provides the features to load a md3 model and build his vertex buffer.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
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
    {$REGION 'Documentation'}
    {**
     The maximum path length supported by the MD3 format
    }
    {$ENDREGION}
    CQR_MD3_MAX_QPATH = 63;

    {$REGION 'Documentation'}
    {**
     The MD3 file version
    }
    {$ENDREGION}
    CQR_MD3_Mesh_File_Version: TQRUInt32 = 15;

    {$REGION 'Documentation'}
    {**
     The MD3 file magic number, that can be used to identify the file content. His value is 'IDP3'
    }
    {$ENDREGION}
    CQR_MD3_ID: TQRUInt32 = ($33 shl 24) + ($50 shl 16) + ($44 shl 8) + $49;

    {$REGION 'Documentation'}
    {**
     The maximum number of frames a MD3 file supports
    }
    {$ENDREGION}
    CQR_MD3_MAX_FRAMES: TQRUInt32 = 1024;

    {$REGION 'Documentation'}
    {**
     The maximum number of tags a MD3 file supports
    }
    {$ENDREGION}
    CQR_MD3_MAX_TAGS: TQRUInt32 = 16;

    {$REGION 'Documentation'}
    {**
     The maximum number of meshes a MD3 file supports
    }
    {$ENDREGION}
    CQR_MD3_MAX_MESHES: TQRUInt32 = 32;

    {$REGION 'Documentation'}
    {**
     The maximum number of shaders a MD3 file supports
    }
    {$ENDREGION}
    CQR_MD3_MAX_SHADERS: TQRUInt32 = 256;

    {$REGION 'Documentation'}
    {**
     The maximum number of vertices a MD3 file supports
    }
    {$ENDREGION}
    CQR_MD3_MAX_VERTICES: TQRUInt32 = 4096;

    {$REGION 'Documentation'}
    {**
     The maximum number of faces a MD3 file supports
    }
    {$ENDREGION}
    CQR_MD3_MAX_FACES: TQRUInt32 = 8192;

    {$REGION 'Documentation'}
    {**
     The global scale factor to apply to all MD3 models
     @br @bold(NOTE) as shown in doc http://www.icculus.org/~phaethon/q3a/formats/md3format.html
    }
    {$ENDREGION}
    CQR_MD3_XYZ_Scale: TQRFloat32 = 64.0;

    {$REGION 'Documentation'}
    {**
     The invalid index error value to use with MD3 files
    }
    {$ENDREGION}
    CQR_MD3_Invalid_Index: NativeUInt = NativeUInt(-1);
    //----------------------------------------------------------------------------------------------

type
    {$REGION 'Documentation'}
    {**
     MD3 header
    }
    {$ENDREGION}
    TQRMD3Header = record
        {$REGION 'Documentation'}
        {**
         MD3 magic number identifier, should be equal to CQR_MD3_ID
        }
        {$ENDREGION}
        m_ID: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         MD3 version, should be equal to 15
        }
        {$ENDREGION}
        m_Version: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         MD3 name, usually it's the path name in the pk3 package file. ASCII character string,
         NULL-terminated (C-style)
        }
        {$ENDREGION}
        m_FileName: array [0..CQR_MD3_MAX_QPATH] of AnsiChar;

        {$REGION 'Documentation'}
        {**
         MD3 flags
        }
        {$ENDREGION}
        m_Flags: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of frame objects, with a maximum of CQR_MD3_MAX_FRAMES
        }
        {$ENDREGION}
        m_FrameCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of tag objects, with a maximum of CQR_MD3_MAX_TAGS
        }
        {$ENDREGION}
        m_TagCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of mesh objects, with a maximum of CQR_MD3_MAX_MESHES
        }
        {$ENDREGION}
        m_MeshCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of skin objects
         @br @bold(NOTE) This appears to be an artifact from the Quake 2 MD2 format. Mesh objects
                         have their own shader field
        }
        {$ENDREGION}
        m_SkinCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Relative offset from start of MD3 object where frame objects start
         @br @bold(NOTE) The frame objects are written sequentially, thus there is no need to seek
                         to the next object after one frame object is read
        }
        {$ENDREGION}
        m_FrameOffset: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Relative offset from start of MD3 where tag objects start
         @br @bold(NOTE) The tag objects are written sequentially, thus there is no need to seek to
                         the next object after one tag object is read
        }
        {$ENDREGION}
        m_TagOffset: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Relative offset from start of MD3 where mesh objects start
         @br @bold(NOTE) The mesh objects are written sequentially, thus there is no need to seek to
                         the next object after one mesh object is read
        }
        {$ENDREGION}
        m_MeshOffset: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Relative offset from start of MD3 to the end of the MD3 object
        }
        {$ENDREGION}
        m_EndOffset: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Header = ^TQRMD3Header;

    {$REGION 'Documentation'}
    {**
     MD3 mesh info
    }
    {$ENDREGION}
    TQRMD3MeshInfo = record
        {$REGION 'Documentation'}
        {**
         MD3 magic number identifier, should be equal to CQR_MD3_ID
        }
        {$ENDREGION}
        m_MeshID: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Mesh name. ASCII character string, NULL-terminated (C-style)
        }
        {$ENDREGION}
        m_Name: array [0..CQR_MD3_MAX_QPATH] of AnsiChar;

        {$REGION 'Documentation'}
        {**
         Mesh flags
        }
        {$ENDREGION}
        m_Flags: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of animation frames. This should match m_FrameCount in the MD3 header
        }
        {$ENDREGION}
        m_AnimationCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of shader objects defined in this mesh, with a limit of CQR_MD3_MAX_SHADERS
        }
        {$ENDREGION}
        m_ShaderCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of vertices defined in this mesh, up to CQR_MD3_MAX_VERTICES
        }
        {$ENDREGION}
        m_VertexCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of faces defined in this mesh, maximum of CQR_MD3_MAX_FACES
        }
        {$ENDREGION}
        m_FaceCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Relative offset from m_MeshOffset where the face list starts
        }
        {$ENDREGION}
        m_FaceOffset: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Relative offset from m_MeshOffset where the shader object list starts
        }
        {$ENDREGION}
        m_ShaderOffset: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Relative offset from m_MeshOffset where the texture coordinates list starts
        }
        {$ENDREGION}
        m_UVOffset: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Relative offset from m_MeshOffset where the polygon list (XYZ vertices + normals) starts
        }
        {$ENDREGION}
        m_PolygonOffset: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Relative offset from m_MeshOffset where the mesh ends
        }
        {$ENDREGION}
        m_MeshEndOffset: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3MeshInfo = ^TQRMD3MeshInfo;

    {$REGION 'Documentation'}
    {**
     MD3 tag. Used to link, rotate and translate the children models
    }
    {$ENDREGION}
    TQRMD3Tag = record
        {$REGION 'Documentation'}
        {**
         Tag name. ASCII character string, NULL-terminated (C-style)
        }
        {$ENDREGION}
        m_Name: array [0..CQR_MD3_MAX_QPATH] of AnsiChar;

        {$REGION 'Documentation'}
        {**
         Tag object coordinates
        }
        {$ENDREGION}
        m_Position: array [0..2] of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         3x3 rotation matrix associated with the tag
        }
        {$ENDREGION}
        m_Rotation: array [0..2] of array [0..2] of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Tag = ^TQRMD3Tag;

    {$REGION 'Documentation'}
    {**
     MD3 frame
    }
    {$ENDREGION}
    TQRMD3Frame = record
        {$REGION 'Documentation'}
        {**
         Minimum edge of the bounding box surrounding the frame
        }
        {$ENDREGION}
        m_Min: array [0..2] of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Maximum edge of the bounding box surrounding the frame
        }
        {$ENDREGION}
        m_Max: array [0..2] of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Bounding sphere origin, usually (0, 0, 0)
        }
        {$ENDREGION}
        m_Origin:  array [0..2] of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Bounding sphere radius
        }
        {$ENDREGION}
        m_Radius:  TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Frame name. ASCII character string, NULL-terminated (C-style)
        }
        {$ENDREGION}
        m_Name: array [0..15] of AnsiChar;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        procedure Read(pBuffer: TStream);
        {$ENDREGION}
    end;

    PQRMD3Frame = ^TQRMD3Frame;

    {$REGION 'Documentation'}
    {**
     MD3 vertex
    }
    {$ENDREGION}
    TQRMD3Vertex = record
        {$REGION 'Documentation'}
        {**
         x, y, and z coordinates in right-handed 3-space, scaled down by factor 1.0 / CQR_MD3_XYZ_Scale
         @br @bold(NOTE) Multiply by 1.0 / CQR_MD3_XYZ_Scale to obtain original coordinate value
        }
        {$ENDREGION}
        m_Position: array [0..2] of TQRInt16;

        {$REGION 'Documentation'}
        {**
         Zenith and azimuth angles of normal vector. 255 corresponds to 2 pi
         @br @bold(NOTE) In MD3 files, the normal vector uses a spherical coordinate system. See
                         https://en.wikipedia.org/wiki/Spherical_coordinate_system for further
                         information
        }
        {$ENDREGION}
        m_Normal: array [0..1] of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Vertex = ^TQRMD3Vertex;

    {$REGION 'Documentation'}
    {**
     MD3 face (or indexed polygon)
    }
    {$ENDREGION}
    TQRMD3Face = record
        m_Indices: array [0..2] of TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Face = ^TQRMD3Face;

    {$REGION 'Documentation'}
    {**
     MD3 texture UV coordinates
    }
    {$ENDREGION}
    TQRMD3TextureCoord = record
        {$REGION 'Documentation'}
        {**
         Texture u and v coordinates
        }
        {$ENDREGION}
        m_Coordinate: array [0..1] of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3TextureCoord = ^TQRMD3TextureCoord;

    {$REGION 'Documentation'}
    {**
     MD3 shader
    }
    {$ENDREGION}
    TQRMD3Shader = record
        {$REGION 'Documentation'}
        {**
         Shader path name in the pk3 package file. ASCII character string, NULL-terminated (C-style)
        }
        {$ENDREGION}
        m_Name: array [0..CQR_MD3_MAX_QPATH] of AnsiChar;

        {$REGION 'Documentation'}
        {**
         Shader index number
        }
        {$ENDREGION}
        m_ShaderIndex: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD3Shader = ^TQRMD3Shader;

    {$REGION 'Documentation'}
    {**
     MD3 mesh
    }
    {$ENDREGION}
    TQRMD3Mesh = record
        {$REGION 'Documentation'}
        {**
         Mesh info, contains the offsets and sizes to read the mesh
        }
        {$ENDREGION}
        m_Info: TQRMD3MeshInfo;

        {$REGION 'Documentation'}
        {**
         Shaders used by the mesh
        }
        {$ENDREGION}
        m_Shaders: array of TQRMD3Shader;

        {$REGION 'Documentation'}
        {**
         Texture coordinates table belonging to mesh
        }
        {$ENDREGION}
        m_TexCoords: array of TQRMD3TextureCoord;

        {$REGION 'Documentation'}
        {**
         Vertex table belonging to mesh
        }
        {$ENDREGION}
        m_Vertices: array of TQRMD3Vertex;

        {$REGION 'Documentation'}
        {**
         Face (or indexed polygon) table belonging to mesh
        }
        {$ENDREGION}
        m_Faces: array of TQRMD3Face;
    end;

    PQRMD3Mesh = ^TQRMD3Mesh;

    {$REGION 'Documentation'}
    {**
     Reads and exposes MD3 file content
    }
    {$ENDREGION}
    TQRMD3Parser = class(TQRModelParser)
        private
            m_Header: TQRMD3Header;
            m_Frames: array of TQRMD3Frame;
            m_Tags:   array of TQRMD3Tag;
            m_Meshes: array of TQRMD3Mesh;

        protected
            {$REGION 'Documentation'}
            {**
             Gets header
             @return(Header, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetHeader: PQRMD3Header; virtual;

            {$REGION 'Documentation'}
            {**
             Gets frame at index
             @param(index Index)
             @return(Frame, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetFrame(index: NativeInt): PQRMD3Frame; virtual;

            {$REGION 'Documentation'}
            {**
             Gets tag at index
             @param(index Index)
             @return(Tag, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetTag(index: NativeInt): PQRMD3Tag; virtual;

            {$REGION 'Documentation'}
            {**
             Gets mesh at index
             @param(index Index)
             @return(Mesh, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetMesh(index: NativeInt): PQRMD3Mesh; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the frame count
             @return(The frame count)
            }
            {$ENDREGION}
            function GetFrameCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the tag count
             @return(The tag count)
            }
            {$ENDREGION}
            function GetTagCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the mesh count
             @return(The mesh count)
            }
            {$ENDREGION}
            function GetMeshCount: NativeInt; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Loads MD3 from file
             @param(fileName File name)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(const fileName: TFileName): Boolean; override;

            {$REGION 'Documentation'}
            {**
             Loads MD3 from buffer
             @param(pBuffer Buffer)
             @param(readLength Length to read in buffer, in bytes (not used here, can be 0))
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Read will begin from current offset
            }
            {$ENDREGION}
            function Load(const pBuffer: TStream; readLength: NativeUInt): Boolean; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the MD3 header
            }
            {$ENDREGION}
            property Header: PQRMD3Header read GetHeader;

            {$REGION 'Documentation'}
            {**
             Gets the frame at index
            }
            {$ENDREGION}
            property Frames[index: NativeInt]: PQRMD3Frame read GetFrame;

            {$REGION 'Documentation'}
            {**
             Gets the tag at index
            }
            {$ENDREGION}
            property Tags[index: NativeInt]: PQRMD3Tag read GetTag;

            {$REGION 'Documentation'}
            {**
             Gets the mesh at index
            }
            {$ENDREGION}
            property Meshes[index: NativeInt]: PQRMD3Mesh read GetMesh;

            {$REGION 'Documentation'}
            {**
             Gets the frame count
            }
            {$ENDREGION}
            property FrameCount: NativeInt read GetFrameCount;

            {$REGION 'Documentation'}
            {**
             Gets the tag count
            }
            {$ENDREGION}
            property TagCount: NativeInt read GetTagCount;

            {$REGION 'Documentation'}
            {**
             Gets the mesh count
            }
            {$ENDREGION}
            property MeshCount: NativeInt read GetMeshCount;
    end;

    TQRMD3Vertices           = array of TQRVector3D;
    TQRMD3Normals            = array of TQRVector3D;
    TQRMD3TexCoords          = array of TQRTexCoord;
    TQRMD3VerticesDictionary = TDictionary<NativeUInt, TQRMD3Vertices>;
    TQRMD3NormalDictionary   = TDictionary<NativeUInt, TQRMD3Normals>;
    TQRMD3TextureDictionary  = TDictionary<NativeUInt, TQRMD3TexCoords>;

    {$REGION 'Documentation'}
    {**
     MD3 model
    }
    {$ENDREGION}
    TQRMD3Model = class(TQRFramedModel)
        private
            m_pParser:    TQRMD3Parser;
            m_pVertices:  TQRMD3VerticesDictionary;
            m_pNormals:   TQRMD3NormalDictionary;
            m_pTexCoords: TQRMD3TextureDictionary;

        protected
            {$REGION 'Documentation'}
            {**
             Prepares mesh to be used by mesh generator
            }
            {$ENDREGION}
            procedure PrepareMesh; virtual;

            {$REGION 'Documentation'}
            {**
             Adds vertex to vertex buffer
             @param(index Mesh index to get)
             @param(indice Vertex indice as read from md3 file)
             @param(vertexFormat Vertex format to apply)
             @param(vertices Vertices list read from md3 file)
             @param(normals Normals list read from md3 file)
             @param(texCoords Texture coordinate list read from md3 file)
             @param(color Vertex color to apply)
             @param(vertex Vertex containing buffer to add to)
            }
            {$ENDREGION}
            procedure AddVertex(index, indice: NativeInt;
                                 vertexFormat: TQRVertexFormat;
                               const vertices: TQRMD3Vertices;
                                const normals: TQRMD3Normals;
                              const texCoords: TQRMD3TexCoords;
                                 const pColor: TQRColor;
                                   var vertex: TQRVertex); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Adds vertex to vertex buffer, interpolate current and next vertex
             @param(index Mesh index to get)
             @param(nextIndex Next mesh index to interpolate with)
             @param(indice Vertex indice as read from md3 file)
             @param(interpolationFactor Interpolation factor to apply)
             @param(vertexFormat Vertex format to apply)
             @param(vertices Vertices list read from md3 file)
             @param(normals Normals list read from md3 file)
             @param(texCoords Texture coordinate list read from md3 file)
             @param(color Vertex color to apply)
             @param(vertex Vertex containing buffer to add to)
            }
            {$ENDREGION}
            procedure AddVertex(index, nextIndex, indice: NativeInt;
                                     interpolationFactor: Single;
                                            vertexFormat: TQRVertexFormat;
                                          const vertices: TQRMD3Vertices;
                                           const normals: TQRMD3Normals;
                                         const texCoords: TQRMD3TexCoords;
                                            const pColor: TQRColor;
                                              var vertex: TQRVertex); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Uncompresses normal
             @param(latitude Normal latitude as written if md3 file)
             @param(longitude Normal longitude as written if md3 file)
             @return(Uncompressed normal)
            }
            {$ENDREGION}
            function UncompressNormal(latitude, longitude: Byte): TQRVector3D; virtual;

            {$REGION 'Documentation'}
            {**
             Gets MD3 parser
             @return(MD3 parser)
            }
            {$ENDREGION}
            function GetParser: TQRMD3Parser; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Loads MD3 from file
             @param(fileName File name)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(const fileName: TFileName): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads MD3 from buffer
             @param(pBuffer Buffer)
             @param(readLength Length to read in buffer, in bytes)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Read will begin from current offset
            }
            {$ENDREGION}
            function Load(const pBuffer: TStream;
                             readLength: NativeUInt): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the model frame mesh
             @param(index Frame mesh index to create)
             @param(mesh @bold([out]) Frame mesh)
             @param(pAABBTree Aligned-axis bounding box tree to populate, ignored if @nil)
             @param(hIsCanceled Callback function that allows to break the operation, can be @nil)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Vertex buffer content is structured as follow:
                             @br [1]x [2]y [3]z [4]nx [5]ny [6]nz [7]tu [8]tv [9]r [10]g [11]b [12]a
                             @br
                             @br where:
                             @br @bold(x/y/z)    - vertex coordinates
                             @br @bold(nx/ny/nz) - vertex normal (if the VertexFormat property contains the EQR_VF_Normals option)
                             @br @bold(tu/tv)    - vertex texture coordinates (if the VertexFormat property contains the EQR_VF_TexCoords option)
                             @br @bold(r/g/b/a)  - vertex color (if the VertexFormat property contains the EQR_VF_Colors option)
            }
            {$ENDREGION}
            function GetMesh(index: NativeUInt;
                          out mesh: TQRMesh;
                         pAABBTree: TQRAABBTree;
                       hIsCanceled: TQRIsCanceledEvent = nil): Boolean; overload; override;

            {$REGION 'Documentation'}
            {**
             Gets the model frame mesh
             @param(index Frame mesh index to get)
             @param(nextIndex Frame mesh index to interpolate with)
             @param(interpolationFactor Interpolation factor to apply)
             @param(mesh @bold([out]) Frame mesh)
             @param(hIsCanceled Callback function that allows to break the operation, can be @nil)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Vertex buffer content is structured as follow:
                             @br [1]x [2]y [3]z [4]nx [5]ny [6]nz [7]tu [8]tv [9]r [10]g [11]b [12]a
                             @br
                             @br where:
                             @br @bold(x/y/z)    - vertex coordinates
                             @br @bold(nx/ny/nz) - vertex normal (if the VertexFormat property contains the EQR_VF_Normals option)
                             @br @bold(tu/tv)    - vertex texture coordinates (if the VertexFormat property contains the EQR_VF_TexCoords option)
                             @br @bold(r/g/b/a)  - vertex color (if the VertexFormat property contains the EQR_VF_Colors option)
            }
            {$ENDREGION}
            function GetMesh(index, nextIndex: NativeUInt;
                          interpolationFactor: Double;
                                     out mesh: TQRMesh;
                                  hIsCanceled: TQRIsCanceledEvent): Boolean; overload; override;

            {$REGION 'Documentation'}
            {**
             Gets mesh count
             @return(Mesh count)
            }
            {$ENDREGION}
            function GetMeshCount: NativeUInt; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the MD3 parser
            }
            {$ENDREGION}
            property Parser: TQRMD3Parser read GetParser;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRMD3Header
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Header.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Header), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(PQRUInt32(m_ID),          SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_Version),     SizeOf(TQRUInt32));
    pBuffer.Read(m_FileName,               SizeOf(m_FileName));
    pBuffer.Read(PQRUInt32(m_Flags),       SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FrameCount),  SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_TagCount),    SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_MeshCount),   SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_SkinCount),   SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FrameOffset), SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_TagOffset),   SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_MeshOffset),  SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_EndOffset),   SizeOf(TQRUInt32));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3MeshInfo
//--------------------------------------------------------------------------------------------------
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
    pBuffer.Read(PQRUInt32(m_AnimationCount), SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_ShaderCount),    SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_VertexCount),    SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FaceCount),      SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FaceOffset),     SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_ShaderOffset),   SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_UVOffset),       SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_PolygonOffset),  SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_MeshEndOffset),  SizeOf(TQRUInt32));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3Tag
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
// TQRMD3Frame
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Frame.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Frame), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_Min,                SizeOf(m_Min));
    pBuffer.Read(m_Max,                SizeOf(m_Max));
    pBuffer.Read(m_Origin,             SizeOf(m_Origin));
    pBuffer.Read(PQRFloat32(m_Radius), SizeOf(TQRFloat32));
    pBuffer.Read(m_Name,               SizeOf(m_Name));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3Vertex
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Vertex.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Vertex), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_Position, SizeOf(m_Position));
    pBuffer.Read(m_Normal,   SizeOf(m_Normal));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3Face
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Face.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Face), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_Indices, SizeOf(m_Indices));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3TextureCoord
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
// TQRMD3Shader
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Shader.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD3Shader), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD3 file to read next data - ' + errorMsg);

    // read data from file
    pBuffer.Read(m_Name,                   SizeOf(m_Name));
    pBuffer.Read(pQRUInt32(m_ShaderIndex), SizeOf(TQRUInt32));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3Parser
//--------------------------------------------------------------------------------------------------
constructor TQRMD3Parser.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3Parser.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Parser.GetHeader: PQRMD3Header;
begin
    Result := @m_Header;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Parser.GetFrame(index: NativeInt): PQRMD3Frame;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Frames))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Frames[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Parser.GetTag(index: NativeInt): PQRMD3Tag;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Tags))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Tags[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Parser.GetMesh(index: NativeInt): PQRMD3Mesh;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Meshes))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Meshes[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Parser.GetFrameCount: NativeInt;
begin
    Result := Length(m_Frames);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Parser.GetTagCount: NativeInt;
begin
    Result := Length(m_Tags);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Parser.GetMeshCount: NativeInt;
begin
    Result := Length(m_Meshes);
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
function TQRMD3Parser.Load(const pBuffer: TStream; readLength: NativeUInt): Boolean;
var
    i, j, tagCount, verticesFrameCount: Cardinal;
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
        SetLength(m_Frames, m_Header.m_FrameCount);

        // read bones
        if (m_Header.m_FrameCount > 0) then
            for i := 0 to m_Header.m_FrameCount - 1 do
                m_Frames[i].Read(pBuffer);

        // get tag count
        tagCount := m_Header.m_FrameCount * m_Header.m_TagCount;

        // create tags, for each animation there is a tag array
        SetLength(m_Tags, tagCount);

        // read tags
        if (tagCount > 0) then
            for i := 0 to tagCount - 1 do
                m_Tags[i].Read(pBuffer);

        // create meshes
        SetLength(m_Meshes, m_Header.m_MeshCount);

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
                m_Meshes[i].m_Info.Read(pBuffer);

                // get vertices count
                verticesFrameCount :=
                        m_Meshes[i].m_Info.m_AnimationCount * m_Meshes[i].m_Info.m_VertexCount;

                // create mesh structure
                SetLength(m_Meshes[i].m_Shaders,   m_Meshes[i].m_Info.m_ShaderCount);
                SetLength(m_Meshes[i].m_Faces,     m_Meshes[i].m_Info.m_FaceCount);
                SetLength(m_Meshes[i].m_TexCoords, m_Meshes[i].m_Info.m_VertexCount);
                SetLength(m_Meshes[i].m_Vertices,  verticesFrameCount);

                // read skins
                if (m_Meshes[i].m_Info.m_ShaderCount > 0) then
                    for j := 0 to m_Meshes[i].m_Info.m_ShaderCount - 1 do
                        m_Meshes[i].m_Shaders[j].Read(pBuffer);

                // go to faces offset
                pBuffer.Seek(offset + m_Meshes[i].m_Info.m_FaceOffset, soBeginning);

                // read faces (also named triangles in many documentation)
                if (m_Meshes[i].m_Info.m_FaceCount > 0) then
                    for j := 0 to m_Meshes[i].m_Info.m_FaceCount - 1 do
                        m_Meshes[i].m_Faces[j].Read(pBuffer);

                // go to texture coords offset
                pBuffer.Seek(offset + m_Meshes[i].m_Info.m_UVOffset, soBeginning);

                // read texture coords
                if (m_Meshes[i].m_Info.m_VertexCount > 0) then
                    for j := 0 to m_Meshes[i].m_Info.m_VertexCount - 1 do
                        m_Meshes[i].m_TexCoords[j].Read(pBuffer);

                // go to polygons offset
                pBuffer.Seek(offset + m_Meshes[i].m_Info.m_PolygonOffset, soBeginning);

                // read polygons (also named vertices in many documentation)
                if (verticesFrameCount > 0) then
                    for j := 0 to verticesFrameCount - 1 do
                        m_Meshes[i].m_Vertices[j].Read(pBuffer);

                // calculate next mesh offset
                Inc(offset, m_Meshes[i].m_Info.m_MeshEndOffset);
            end;

        Result := True;
        Exit;
    except
        on e: Exception do ; // ignore any error
    end;

    Result := False;
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3Model
//--------------------------------------------------------------------------------------------------
constructor TQRMD3Model.Create;
begin
    inherited Create;

    m_pParser    := TQRMD3Parser.Create;
    m_pVertices  := TQRMD3VerticesDictionary.Create;
    m_pNormals   := TQRMD3NormalDictionary.Create;
    m_pTexCoords := TQRMD3TextureDictionary.Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3Model.Destroy;
begin
    // clear memory
    m_pParser.Free;
    m_pVertices.Free;
    m_pNormals.Free;
    m_pTexCoords.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
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
                m_pParser.m_Meshes[i].m_Info.m_VertexCount *
                m_pParser.m_Meshes[i].m_Info.m_AnimationCount;

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
                vertices[j].X := m_pParser.m_Meshes[i].m_Vertices[j].m_Position[0] / CQR_MD3_XYZ_Scale;
                vertices[j].Y := m_pParser.m_Meshes[i].m_Vertices[j].m_Position[1] / CQR_MD3_XYZ_Scale;
                vertices[j].Z := m_pParser.m_Meshes[i].m_Vertices[j].m_Position[2] / CQR_MD3_XYZ_Scale;

                // uncompress normal and add it to temporary normal list
                normals[j] := UncompressNormal(m_pParser.m_Meshes[i].m_Vertices[j].m_Normal[0],
                                               m_pParser.m_Meshes[i].m_Vertices[j].m_Normal[1]);
            end;

            m_pVertices.Add(i, vertices);
            m_pNormals.Add(i, normals);
        finally
            SetLength(vertices, 0);
            SetLength(normals, 0);
        end;

        // get texture coordinates count
        texCoordCount := m_pParser.m_Meshes[i].m_Info.m_VertexCount;

        try
            // reserve memory for temporary texture coordinates to get from md3 file
            SetLength(texCoords, texCoordCount);

            // iterate through texture coordinates to get
            for j := 0 to texCoordCount - 1 do
            begin
                // uncompress texture coordinates and add them to temporary texure coord list
                texCoords[j].U := m_pParser.m_Meshes[i].m_TexCoords[j].m_Coordinate[0];
                texCoords[j].V := m_pParser.m_Meshes[i].m_TexCoords[j].m_Coordinate[1];
            end;

            m_pTexCoords.Add(i, texCoords);
        finally
            SetLength(texCoords, 0);
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
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
    if (EQR_VF_Normals in VertexFormat) then
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
    if (EQR_VF_TexCoords in VertexFormat) then
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
    if (EQR_VF_Colors in VertexFormat) then
    begin
        // add memory for texture coordinates
        SetLength(vertex.m_Buffer, offset + 4);

        vertex.m_Buffer[offset]     := color.GetRedF;
        vertex.m_Buffer[offset + 1] := color.GetGreenF;
        vertex.m_Buffer[offset + 2] := color.GetBlueF;
        vertex.m_Buffer[offset + 3] := color.GetAlphaF;
    end;
end;
//--------------------------------------------------------------------------------------------------
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
    if (EQR_VF_Normals in VertexFormat) then
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
    if (EQR_VF_TexCoords in VertexFormat) then
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
    if (EQR_VF_Colors in VertexFormat) then
    begin
        vertex.m_Buffer[offset]     := color.GetRedF;
        vertex.m_Buffer[offset + 1] := color.GetGreenF;
        vertex.m_Buffer[offset + 2] := color.GetBlueF;
        vertex.m_Buffer[offset + 3] := color.GetAlphaF;
    end;
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
function TQRMD3Model.GetParser: TQRMD3Parser;
begin
    Result := m_pParser;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Model.Load(const fileName: TFileName): Boolean;
begin
    Result := m_pParser.Load(fileName);

    if (Result) then
        PrepareMesh;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Model.Load(const pBuffer: TStream; readLength: NativeUInt): Boolean;
begin
    Result := m_pParser.Load(pBuffer, readLength);

    if (Result) then
        PrepareMesh;
end;
//--------------------------------------------------------------------------------------------------
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
    if (index >= GetMeshCount) then
    begin
        Result := False;
        Exit;
    end;

    // basically stride is the coordinates values size
    stride := 3;

    // do include normals?
    if (EQR_VF_Normals in VertexFormat) then
        Inc(stride, 3);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in VertexFormat) then
        Inc(stride, 2);

    // do include colors?
    if (EQR_VF_Colors in VertexFormat) then
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

        mesh[meshIndex].m_Name      := UnicodeString(AnsiString(m_pParser.m_Meshes[i].m_Info.m_Name));
        mesh[meshIndex].m_Stride    := stride;
        mesh[meshIndex].m_Type      := EQR_VT_Triangles;
        mesh[meshIndex].m_Format    := VertexFormat;
        mesh[meshIndex].m_CoordType := EQR_VC_XYZ;

        // get indice count
        indiceCount := m_pParser.m_Meshes[i].m_Info.m_FaceCount;

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
                AddVertex((index * m_pParser.m_Meshes[i].m_Info.m_VertexCount),
                          m_pParser.m_Meshes[i].m_Faces[j].m_Indices[k],
                          VertexFormat,
                          m_pVertices.Items[i],
                          m_pNormals.Items[i],
                          m_pTexCoords.Items[i],
                          Color,
                          mesh[meshIndex]);
    end;

    // populate aligned-axis bounding box tree
    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
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
    if (index >= GetMeshCount) then
    begin
        Result := False;
        Exit;
    end;

    // basically stride is the coordinates values size
    stride := 3;

    // do include normals?
    if (EQR_VF_Normals in VertexFormat) then
        Inc(stride, 3);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in VertexFormat) then
        Inc(stride, 2);

    // do include colors?
    if (EQR_VF_Colors in VertexFormat) then
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

        mesh[meshIndex].m_Name      := UnicodeString(AnsiString(m_pParser.m_Meshes[i].m_Info.m_Name));
        mesh[meshIndex].m_Stride    := stride;
        mesh[meshIndex].m_Type      := EQR_VT_Triangles;
        mesh[meshIndex].m_Format    := VertexFormat;
        mesh[meshIndex].m_CoordType := EQR_VC_XYZ;

        // get indice count
        indiceCount := m_pParser.m_Meshes[i].m_Info.m_FaceCount;

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
                AddVertex((index     * m_pParser.m_Meshes[i].m_Info.m_VertexCount),
                          (nextIndex * m_pParser.m_Meshes[i].m_Info.m_VertexCount),
                          m_pParser.m_Meshes[i].m_Faces[j].m_Indices[k],
                          interpolationFactor,
                          VertexFormat,
                          m_pVertices.Items[i],
                          m_pNormals.Items[i],
                          m_pTexCoords.Items[i],
                          Color,
                          mesh[meshIndex]);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Model.GetMeshCount: NativeUInt;
begin
    Result := m_pParser.m_Header.m_FrameCount;
end;
//--------------------------------------------------------------------------------------------------

end.
