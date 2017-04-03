// *************************************************************************************************
// * ==> UTQRMDL ----------------------------------------------------------------------------------*
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
 @abstract(@name provides the features to load a MDL model and build his vertex buffer.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRMDL;

{$MODE Delphi}

interface

uses Classes,
     SysUtils,
     UTQRCommon,
     UTQRHelpers,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRLight,
     UTQRCollision,
     UTQRModel;

const
    //----------------------------------------------------------------------------------------------
    // Global constants
    //----------------------------------------------------------------------------------------------
    {$REGION 'Documentation'}
    {**
     The MDL file version
    }
    {$ENDREGION}
    CQR_MDL_Mesh_File_Version: TQRUInt32 = 6;

    {$REGION 'Documentation'}
    {**
     The MDL file magic number, that can be used to identify the file content. His value is 'IDPO'
    }
    {$ENDREGION}
    CQR_MDL_ID: TQRUInt32 = ($4F shl 24) + ($50 shl 16) + ($44 shl 8) + $49;

    {$REGION 'Documentation'}
    {**
     The invalid index error value to use with MDL files
    }
    {$ENDREGION}
    CQR_MDL_Invalid_Index: NativeUInt = NativeUInt(-1);
    //----------------------------------------------------------------------------------------------

type
    {$REGION 'Documentation'}
    {**
     MDL header
    }
    {$ENDREGION}
    TQRMDLHeader = record
        {$REGION 'Documentation'}
        {**
         MDL magic number identifier, should be equal to CQR_MDL_ID
        }
        {$ENDREGION}
        m_ID: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         MDL version, should be equal to 8
        }
        {$ENDREGION}
        m_Version: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Scale factor
        }
        {$ENDREGION}
        m_Scale: array [0..2] of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Translation vector
        }
        {$ENDREGION}
        m_Translate: array [0..2] of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Bounding sphere radius
        }
        {$ENDREGION}
        m_BoundingRadius: TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Eye's position
        }
        {$ENDREGION}
        m_EyePosition: array [0..2] of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Number of textures
        }
        {$ENDREGION}
        m_SkinCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         MDL texture width
        }
        {$ENDREGION}
        m_SkinWidth: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         MDL texture height
        }
        {$ENDREGION}
        m_SkinHeight: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of vertices
        }
        {$ENDREGION}
        m_VertexCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Number of polygons
        }
        {$ENDREGION}
        m_PolygonCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Total number of frames
        }
        {$ENDREGION}
        m_FrameCount: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Synchronization type, 0 = synchrone, 1 = random
        }
        {$ENDREGION}
        m_SyncType: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Flags
        }
        {$ENDREGION}
        m_Flags: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Size of model
        }
        {$ENDREGION}
        m_Size: TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMDLHeader = ^TQRMDLHeader;

    {$REGION 'Documentation'}
    {**
     MDL skin
    }
    {$ENDREGION}
    TQRMDLSkin = record
        {$REGION 'Documentation'}
        {**
         Is group, 0 = single, 1 = group
        }
        {$ENDREGION}
        m_Group: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Texture count
        }
        {$ENDREGION}
        m_Count: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Length of one texture
        }
        {$ENDREGION}
        m_TexLen: TQRUInt64;

        {$REGION 'Documentation'}
        {**
         Time duration for each texture
        }
        {$ENDREGION}
        m_Times: array of TQRFloat32;

        {$REGION 'Documentation'}
        {**
         Texture data
        }
        {$ENDREGION}
        m_Data: array of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @param(header MDL file header)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream; const header: TQRMDLHeader);
    end;

    PQRMDLSkin= ^TQRMDLSkin;

    {$REGION 'Documentation'}
    {**
     MDL texture coordinate
    }
    {$ENDREGION}
    TQRMDLTextureCoord = record
        {$REGION 'Documentation'}
        {**
         Whether the texture is on the boundary of two pieces
         @br @bold(NOTE) Texture are generally divided in two pieces, one for the frontface of the
                         model, and one for the backface. The backface piece must be translated of
                         SkinWidth / 2 from the frontface piece
        }
        {$ENDREGION}
        m_OnSeam: TQRInt32;

        {$REGION 'Documentation'}
        {**
         Texture u coordinate
        }
        {$ENDREGION}
        m_U: TQRInt32;

        {$REGION 'Documentation'}
        {**
         Texture v coordinate
        }
        {$ENDREGION}
        m_V: TQRInt32;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMDLTextureCoord = ^TQRMDLTextureCoord;

    {$REGION 'Documentation'}
    {**
     MDL polygon
    }
    {$ENDREGION}
    TQRMDLPolygon = record
        {$REGION 'Documentation'}
        {**
         Face culling, 0 = backface, 1 = frontface
        }
        {$ENDREGION}
        m_FacesFront: TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Index of each vertex composing the polygon in the frame buffer
        }
        {$ENDREGION}
        m_VertexIndex: array[0..2] of TQRUInt32;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMDLPolygon = ^TQRMDLPolygon;

    {$REGION 'Documentation'}
    {**
     MDL vertex
    }
    {$ENDREGION}
    TQRMDLVertex = record
        {$REGION 'Documentation'}
        {**
         Vertex coordinates
        }
        {$ENDREGION}
        m_Vertex: array[0..2] of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Index of the normal to use in the normal table
        }
        {$ENDREGION}
        m_NormalIndex: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream);
    end;

    PQRMDLVertex = ^TQRMDLVertex;

    {$REGION 'Documentation'}
    {**
     MDL frame
    }
    {$ENDREGION}
    TQRMDLFrame = record
        {$REGION 'Documentation'}
        {**
         Frame bounding box minimum edge
        }
        {$ENDREGION}
        m_BoundingBoxMin: TQRMDLVertex;

        {$REGION 'Documentation'}
        {**
         Frame bounding box maximum edge
        }
        {$ENDREGION}
        m_BoundingBoxMax: TQRMDLVertex;

        {$REGION 'Documentation'}
        {**
         Frame name
        }
        {$ENDREGION}
        m_Name: string;

        {$REGION 'Documentation'}
        {**
         Vertices that compose the frame
        }
        {$ENDREGION}
        m_Vertices: array of TQRMDLVertex;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @param(header MDL file header)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream; const header: TQRMDLHeader);
    end;

    PQRMDLFrame = ^TQRMDLFrame;

    {$REGION 'Documentation'}
    {**
     MDL frame group
    }
    {$ENDREGION}
    TQRMDLFrameGroup = record
        m_Type:   TQRUInt32;
        m_Count:  TQRUInt32;
        m_Min:    TQRMDLVertex;
        m_Max:    TQRMDLVertex;
        m_Times:  array of TQRFloat32;
        m_Frames: array of TQRMDLFrame;

        {$REGION 'Documentation'}
        {**
         Reads data from file
         @param(pBuffer Buffer to read from)
         @param(header MDL file header)
         @raises(Exception on error)
        }
        {$ENDREGION}
        procedure Read(pBuffer: TStream; const header: TQRMDLHeader);
    end;

    {$REGION 'Documentation'}
    {**
     Reads and exposes MDL file content
    }
    {$ENDREGION}
    TQRMDLParser = class(TQRModelParser)
        private
            m_Header:    TQRMDLHeader;
            m_Skins:     array of TQRMDLSkin;
            m_TexCoords: array of TQRMDLTextureCoord;
            m_Polygons:  array of TQRMDLPolygon;
            m_Frames:    array of TQRMDLFrameGroup;

        protected
            {$REGION 'Documentation'}
            {**
             Gets header
             @return(Header, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetHeader: PQRMDLHeader; virtual;

            {$REGION 'Documentation'}
            {**
             Gets skin at index
             @param(index Index)
             @return(Skin, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetSkin(index: NativeInt): PQRMDLSkin; virtual;

            {$REGION 'Documentation'}
            {**
             Gets texture coordinate at index
             @param(index Index)
             @return(Texture coordinate, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetTexCoord(index: NativeInt): PQRMDLTextureCoord; virtual;

            {$REGION 'Documentation'}
            {**
             Gets polygon at index
             @param(index Index)
             @return(Polygon, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetPolygon(index: NativeInt): PQRMDLPolygon; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the frame at index
             @param(index Index)
             @return(Frame, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetFrame(index: NativeInt): PQRMDLFrame; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the skin count
             @return(The skin count)
            }
            {$ENDREGION}
            function GetSkinCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the texture coordinate count
             @return(The texture coordinate count)
            }
            {$ENDREGION}
            function GetTexCoordCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the polygon count
             @return(The polygon count)
            }
            {$ENDREGION}
            function GetPolygonCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the frame count
             @return(The frame count)
            }
            {$ENDREGION}
            function GetFrameCount: NativeInt; virtual;

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
             Loads MDL from buffer
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
             Gets the MDL header
            }
            {$ENDREGION}
            property Header: PQRMDLHeader read GetHeader;

            {$REGION 'Documentation'}
            {**
             Gets the MDL skin at index
            }
            {$ENDREGION}
            property Skins[index: NativeInt]: PQRMDLSkin read GetSkin;

            {$REGION 'Documentation'}
            {**
             Gets the MDL texture coordinate at index
            }
            {$ENDREGION}
            property TexCoords[index: NativeInt]: PQRMDLTextureCoord read GetTexCoord;

            {$REGION 'Documentation'}
            {**
             Gets the MDL polygon at index
            }
            {$ENDREGION}
            property Polygons[index: NativeInt]: PQRMDLPolygon read GetPolygon;

            {$REGION 'Documentation'}
            {**
             Gets the MDL frame at index
            }
            {$ENDREGION}
            property Frames[index: NativeInt]: PQRMDLFrame read GetFrame;

            {$REGION 'Documentation'}
            {**
             Gets the MDL skin count
            }
            {$ENDREGION}
            property SkinCount: NativeInt read GetSkinCount;

            {$REGION 'Documentation'}
            {**
             Gets the MDL texture coordinate count
            }
            {$ENDREGION}
            property TexCoordCount: NativeInt read GetTexCoordCount;

            {$REGION 'Documentation'}
            {**
             Gets the MDL polygon count
            }
            {$ENDREGION}
            property PolygonCount: NativeInt read GetPolygonCount;

            {$REGION 'Documentation'}
            {**
             Gets the MDL frame count
            }
            {$ENDREGION}
            property FrameCount: NativeInt read GetFrameCount;
    end;

    TQRMDLNormals = array of TQRVector3D;

    {$REGION 'Documentation'}
    {**
     MDL model
    }
    {$ENDREGION}
    TQRMDLModel = class(TQRFramedModel)
        private
            m_pParser:             TQRMDLParser;
            m_Normals:             TQRMDLNormals;
            m_RHToLH:              Boolean;
            m_pPreCalculatedLight: TQRDirectionalLight;

            {$REGION 'Documentation'}
            {**
             Populates the normals table
            }
            {$ENDREGION}
            procedure PopulateNormals;

        protected
            {$REGION 'Documentation'}
            {**
             Uncompresses MDL vertex
             @param(header MDL header)
             @param(pVertex MDL vertex to uncompress)
             @return(The uncompressed vertex)
            }
            {$ENDREGION}
            function UncompressVertex(const header: TQRMDLHeader;
                                      const vertex: TQRMDLVertex): TQRVector3D; virtual;

            {$REGION 'Documentation'}
            {**
             Gets pre-calculated light
             @return(The pre-calculated light)
            }
            {$ENDREGION}
            function GetPreCalculatedLight: TQRDirectionalLight; virtual;

            {$REGION 'Documentation'}
            {**
             Sets pre-calculated light
             @param(pLight Pre-calculated light)
            }
            {$ENDREGION}
            procedure SetPreCalculatedLight(const pLight: TQRDirectionalLight); virtual;

            {$REGION 'Documentation'}
            {**
             Gets left hand to right hand conversion mode flag status
             @return(Left hand to right hand conversion mode flag status)
            }
            {$ENDREGION}
            function GetRHToLH: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Sets left hand to right hand conversion mode flag status
             @param(value Left hand to right hand conversion mode flag status to set)
            }
            {$ENDREGION}
            procedure SetRHToLH(value: Boolean); virtual;

            {$REGION 'Documentation'}
            {**
             Gets MDL parser
             @return(The MDL parser)
            }
            {$ENDREGION}
            function GetParser: TQRMDLParser; virtual;

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
             Loads MDL from file
             @param(fileName File name)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(const fileName: TFileName): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads MDL from buffer
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
             @br @bold(NOTE) vertex buffer content is structured as follow:
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
             @br @bold(NOTE) vertex buffer content is structured as follow:
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
             Gets the mesh count
             @returns(The mesh count)
            }
            {$ENDREGION}
            function GetMeshCount: NativeUInt; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the pre-calculated light to use
            }
            {$ENDREGION}
            property PreCalculatedLight: TQRDirectionalLight read GetPreCalculatedLight write SetPreCalculatedLight;

            {$REGION 'Documentation'}
            {**
             Gets or sets the left hand to right hand converter enabled status
            }
            {$ENDREGION}
            property RHToLH: Boolean read GetRHToLH write SetRHToLH;

            {$REGION 'Documentation'}
            {**
             Gets the MDL parser
            }
            {$ENDREGION}
            property Parser: TQRMDLParser read GetParser;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRMDLHeader
//--------------------------------------------------------------------------------------------------
procedure TQRMDLHeader.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMDLHeader), errorMsg)) then
        raise Exception.Create('Not enough bytes in MDL file to read next data - ' +
                               AnsiString(errorMsg));

    {$IFDEF CPUX64}
        // read header from file
        pBuffer.Read(m_ID,             SizeOf(TQRUInt32));
        pBuffer.Read(m_Version,        SizeOf(TQRUInt32));
        pBuffer.Read(m_Scale[0],       SizeOf(m_Scale));
        pBuffer.Read(m_Translate[0],   SizeOf(m_Translate));
        pBuffer.Read(m_BoundingRadius, SizeOf(TQRFloat32));
        pBuffer.Read(m_EyePosition[0], SizeOf(m_EyePosition));
        pBuffer.Read(m_SkinCount,      SizeOf(TQRUInt32));
        pBuffer.Read(m_SkinWidth,      SizeOf(TQRUInt32));
        pBuffer.Read(m_SkinHeight,     SizeOf(TQRUInt32));
        pBuffer.Read(m_VertexCount,    SizeOf(TQRUInt32));
        pBuffer.Read(m_PolygonCount,   SizeOf(TQRUInt32));
        pBuffer.Read(m_FrameCount,     SizeOf(TQRUInt32));
        pBuffer.Read(m_SyncType,       SizeOf(TQRUInt32));
        pBuffer.Read(m_Flags,          SizeOf(TQRUInt32));
        pBuffer.Read(m_Size,           SizeOf(TQRFloat32));
    {$ELSE}
        // read header from file
        pBuffer.Read(PQRUInt32(m_ID),           SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_Version),      SizeOf(TQRUInt32));
        pBuffer.Read(m_Scale[0],                SizeOf(m_Scale));
        pBuffer.Read(m_Translate[0],            SizeOf(m_Translate));
        pBuffer.Read(m_BoundingRadius,          SizeOf(TQRFloat32));
        pBuffer.Read(m_EyePosition[0],          SizeOf(m_EyePosition));
        pBuffer.Read(PQRUInt32(m_SkinCount),    SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_SkinWidth),    SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_SkinHeight),   SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_VertexCount),  SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_PolygonCount), SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_FrameCount),   SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_SyncType),     SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_Flags),        SizeOf(TQRUInt32));
        pBuffer.Read(m_Size,                    SizeOf(TQRFloat32));
    {$ENDIF}
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLSkin
//--------------------------------------------------------------------------------------------------
procedure TQRMDLSkin.Read(pBuffer: TStream; const header: TQRMDLHeader);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, 64 * SizeOf(AnsiChar), errorMsg)) then
        raise Exception.Create('Not enough bytes in MDL file to read next data - ' +
                               AnsiString(errorMsg));

    SetLength(m_Times, 0);
    SetLength(m_Data, 0);

    // calculate texture size
    m_TexLen := NativeInt(header.m_SkinWidth * header.m_SkinHeight);

    {$IFDEF CPUX64}
        pBuffer.Read(m_Group, SizeOf(TQRUInt32));
    {$ELSE}
        pBuffer.Read(PQRUInt32(m_Group), SizeOf(TQRUInt32));
    {$ENDIF}

    // is a group of textures?
    if (m_Group = 0) then
    begin
        m_Count := 1;

        // create memory for texture
        SetLength(m_Data, m_TexLen);

        // read texture from file
        pBuffer.Read(m_Data[0], m_TexLen);

        Exit;
    end;

    {$IFDEF CPUX64}
        pBuffer.Read(m_Count, SizeOf(TQRUInt32));
    {$ELSE}
        pBuffer.Read(PQRUInt32(m_Count), SizeOf(TQRUInt32));
    {$ENDIF}

    if (m_Count = 0) then
        Exit;

    // create memory for time table
    SetLength(m_Times, m_Count);

    // read time table from file
    pBuffer.Read(m_Times[0], SizeOf(TQRFloat32) * m_Count);

    // create memory for texture
    SetLength(m_Data, m_TexLen * m_Count);

    // read texture from file
    pBuffer.Read(m_Data[0], m_TexLen * m_Count);
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLTextureCoord
//--------------------------------------------------------------------------------------------------
procedure TQRMDLTextureCoord.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMDLTextureCoord), errorMsg)) then
        raise Exception.Create('Not enough bytes in MDL file to read next data - ' +
                               AnsiString(errorMsg));

    // read texture coordinates from file
    {$IFDEF CPUX64}
        pBuffer.Read(m_OnSeam, SizeOf(TQRUInt32));
        pBuffer.Read(m_U,      SizeOf(TQRUInt32));
        pBuffer.Read(m_V,      SizeOf(TQRUInt32));
    {$ELSE}
        pBuffer.Read(PQRUInt32(m_OnSeam), SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_U),      SizeOf(TQRUInt32));
        pBuffer.Read(PQRUInt32(m_V),      SizeOf(TQRUInt32));
    {$ENDIF}
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLPolygon
//--------------------------------------------------------------------------------------------------
procedure TQRMDLPolygon.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMDLPolygon), errorMsg)) then
        raise Exception.Create('Not enough bytes in MDL file to read next data - ' +
                               AnsiString(errorMsg));

    // read polygon from file
    {$IFDEF CPUX64}
        pBuffer.Read(m_FacesFront, SizeOf(TQRUInt32));
    {$ELSE}
        pBuffer.Read(PQRUInt32(m_FacesFront), SizeOf(TQRUInt32));
    {$ENDIF}
    pBuffer.Read(m_VertexIndex[0], SizeOf(m_VertexIndex));
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLVertex
//--------------------------------------------------------------------------------------------------
procedure TQRMDLVertex.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMDLVertex), errorMsg)) then
        raise Exception.Create('Not enough bytes in MDL file to read next data - ' +
                               AnsiString(errorMsg));

    // read vertex from file
    pBuffer.Read(m_Vertex[0],   SizeOf(m_Vertex));
    pBuffer.Read(m_NormalIndex, SizeOf(TQRUInt8));
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLFrame
//--------------------------------------------------------------------------------------------------
procedure TQRMDLFrame.Read(pBuffer: TStream; const header: TQRMDLHeader);
var
    dataLength: NativeUInt;
    name:       TQRAnsiCharArray;
    i:          Cardinal;
    errorMsg:   UnicodeString;
begin
    SetLength(m_Vertices, 0);

    // read frame bounding box
    m_BoundingBoxMin.Read(pBuffer);
    m_BoundingBoxMax.Read(pBuffer);

    // calculate needed length to read next data in buffer
    dataLength := (16 * SizeOf(AnsiChar));

    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, dataLength, errorMsg)) then
        raise Exception.Create('Not enough bytes in MDL file to read next data - ' +
                               AnsiString(errorMsg));

    // reserve memory for char array
    SetLength(name, 16);

    try
        // initialize char array memory filling it with 0
        FillChar(name[0], dataLength, $00);

        // read frame name
        pBuffer.Read(name[0], Length(name) * SizeOf(AnsiChar));

        // set frame name
        m_Name := TQRStringHelper.AnsiCharArrayToStr(name);
    finally
        // clear memory
        SetLength(name, 0);
    end;

    // create frame vertex buffer
    SetLength(m_Vertices, header.m_VertexCount);

    // read frame vertices
    for i := 0 to header.m_VertexCount - 1 do
        m_Vertices[i].Read(pBuffer);
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLFrameGroup
//--------------------------------------------------------------------------------------------------
procedure TQRMDLFrameGroup.Read(pBuffer: TStream; const header: TQRMDLHeader);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRUInt32), errorMsg)) then
        raise Exception.Create('Not enough bytes in MDL file to read next data - ' +
                               AnsiString(errorMsg));

    {$IFDEF CPUX64}
        pBuffer.Read(m_Type, SizeOf(TQRUInt32));
    {$ELSE}
        pBuffer.Read(PQRUInt32(m_Type), SizeOf(TQRUInt32));
    {$ENDIF}

    // is a single frame or a group of frames?
    if (m_Type = 0) then
    begin
        m_Count := 1;

        // create frame vertex buffer
        SetLength(m_Frames, m_Count);

        m_Frames[0].Read(pBuffer, header);
        Exit;
    end;

    raise Exception.Create('NOT IMPLEMENTED');
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLParser
//--------------------------------------------------------------------------------------------------
constructor TQRMDLParser.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMDLParser.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.GetHeader: PQRMDLHeader;
begin
    Result := @m_Header;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.GetSkin(index: NativeInt): PQRMDLSkin;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Skins))) then
        Exit(nil);

    Result := @m_Skins[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.GetTexCoord(index: NativeInt): PQRMDLTextureCoord;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_TexCoords))) then
        Exit(nil);

    Result := @m_TexCoords[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.GetPolygon(index: NativeInt): PQRMDLPolygon;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Polygons))) then
        Exit(nil);

    Result := @m_Polygons[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.GetFrame(index: NativeInt): PQRMDLFrame;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Frames))) then
        Exit(nil);

    Result := @m_Frames[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.GetSkinCount: NativeInt;
begin
    Result := Length(m_Skins);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.GetTexCoordCount: NativeInt;
begin
    Result := Length(m_TexCoords);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.GetPolygonCount: NativeInt;
begin
    Result := Length(m_Polygons);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.GetFrameCount: NativeInt;
begin
    Result := Length(m_Frames);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLParser.Load(const pBuffer: TStream; readLength: NativeUInt): Boolean;
var
    i: NativeUInt;
begin
    try
        // is buffer empty?
        if (pBuffer.Size = 0) then
            Exit(False);

        // read file header
        m_Header.Read(pBuffer);

        // is MDL file and version correct?
        if ((m_Header.m_ID <> CQR_MDL_ID) or (m_Header.m_Version <> CQR_MDL_Mesh_File_Version)) then
            Exit(False);

        // read skins
        if (m_Header.m_SkinCount > 0) then
        begin
            SetLength(m_Skins, m_Header.m_SkinCount);

            for i := 0 to m_Header.m_SkinCount - 1 do
                m_Skins[i].Read(pBuffer, m_Header);
        end;

        // read texture coordinates
        if (m_Header.m_VertexCount > 0) then
        begin
            SetLength(m_TexCoords, m_Header.m_VertexCount);

            for i := 0 to m_Header.m_VertexCount - 1 do
                m_TexCoords[i].Read(pBuffer);
        end;

        // read polygons
        if (m_Header.m_PolygonCount > 0) then
        begin
            SetLength(m_Polygons, m_Header.m_PolygonCount);

            for i := 0 to m_Header.m_PolygonCount - 1 do
                m_Polygons[i].Read(pBuffer);
        end;

        // read frames
        if (m_Header.m_FrameCount > 0) then
        begin
            SetLength(m_Frames, m_Header.m_FrameCount);

            for i := 0 to m_Header.m_FrameCount - 1 do
                m_Frames[i].Read(pBuffer, m_Header);
        end;

        Exit(True);
    except
        on e: Exception do ; // ignore any error
    end;

    Result := False;
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLModel
//--------------------------------------------------------------------------------------------------
constructor TQRMDLModel.Create;
begin
    inherited Create;

    m_pParser             := TQRMDLParser.Create;
    m_pPreCalculatedLight := TQRDirectionalLight.Create;
    m_RHToLH              := False;

    PopulateNormals;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMDLModel.Destroy;
begin
    // clear memory
    SetLength(m_Normals, 0);
    m_pPreCalculatedLight.Free;
    m_pParser.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLModel.PopulateNormals;
type
    {**
     Internal record representing a normal
    }
    IQRNormal = record
        m_X: Single;
        m_Y: Single;
        m_Z: Single;
    end;

const
    {**
     Normal table used by the MDL models
    }
    normals: array[0..161] of IQRNormal =
    (
        (m_X: -0.525731; m_Y:  0.000000; m_Z:  0.850651),
        (m_X: -0.442863; m_Y:  0.238856; m_Z:  0.864188),
        (m_X: -0.295242; m_Y:  0.000000; m_Z:  0.955423),
        (m_X: -0.309017; m_Y:  0.500000; m_Z:  0.809017),
        (m_X: -0.162460; m_Y:  0.262866; m_Z:  0.951056),
        (m_X:  0.000000; m_Y:  0.000000; m_Z:  1.000000),
        (m_X:  0.000000; m_Y:  0.850651; m_Z:  0.525731),
        (m_X: -0.147621; m_Y:  0.716567; m_Z:  0.681718),
        (m_X:  0.147621; m_Y:  0.716567; m_Z:  0.681718),
        (m_X:  0.000000; m_Y:  0.525731; m_Z:  0.850651),
        (m_X:  0.309017; m_Y:  0.500000; m_Z:  0.809017),
        (m_X:  0.525731; m_Y:  0.000000; m_Z:  0.850651),
        (m_X:  0.295242; m_Y:  0.000000; m_Z:  0.955423),
        (m_X:  0.442863; m_Y:  0.238856; m_Z:  0.864188),
        (m_X:  0.162460; m_Y:  0.262866; m_Z:  0.951056),
        (m_X: -0.681718; m_Y:  0.147621; m_Z:  0.716567),
        (m_X: -0.809017; m_Y:  0.309017; m_Z:  0.500000),
        (m_X: -0.587785; m_Y:  0.425325; m_Z:  0.688191),
        (m_X: -0.850651; m_Y:  0.525731; m_Z:  0.000000),
        (m_X: -0.864188; m_Y:  0.442863; m_Z:  0.238856),
        (m_X: -0.716567; m_Y:  0.681718; m_Z:  0.147621),
        (m_X: -0.688191; m_Y:  0.587785; m_Z:  0.425325),
        (m_X: -0.500000; m_Y:  0.809017; m_Z:  0.309017),
        (m_X: -0.238856; m_Y:  0.864188; m_Z:  0.442863),
        (m_X: -0.425325; m_Y:  0.688191; m_Z:  0.587785),
        (m_X: -0.716567; m_Y:  0.681718; m_Z: -0.147621),
        (m_X: -0.500000; m_Y:  0.809017; m_Z: -0.309017),
        (m_X: -0.525731; m_Y:  0.850651; m_Z:  0.000000),
        (m_X:  0.000000; m_Y:  0.850651; m_Z: -0.525731),
        (m_X: -0.238856; m_Y:  0.864188; m_Z: -0.442863),
        (m_X:  0.000000; m_Y:  0.955423; m_Z: -0.295242),
        (m_X: -0.262866; m_Y:  0.951056; m_Z: -0.162460),
        (m_X:  0.000000; m_Y:  1.000000; m_Z:  0.000000),
        (m_X:  0.000000; m_Y:  0.955423; m_Z:  0.295242),
        (m_X: -0.262866; m_Y:  0.951056; m_Z:  0.162460),
        (m_X:  0.238856; m_Y:  0.864188; m_Z:  0.442863),
        (m_X:  0.262866; m_Y:  0.951056; m_Z:  0.162460),
        (m_X:  0.500000; m_Y:  0.809017; m_Z:  0.309017),
        (m_X:  0.238856; m_Y:  0.864188; m_Z: -0.442863),
        (m_X:  0.262866; m_Y:  0.951056; m_Z: -0.162460),
        (m_X:  0.500000; m_Y:  0.809017; m_Z: -0.309017),
        (m_X:  0.850651; m_Y:  0.525731; m_Z:  0.000000),
        (m_X:  0.716567; m_Y:  0.681718; m_Z:  0.147621),
        (m_X:  0.716567; m_Y:  0.681718; m_Z: -0.147621),
        (m_X:  0.525731; m_Y:  0.850651; m_Z:  0.000000),
        (m_X:  0.425325; m_Y:  0.688191; m_Z:  0.587785),
        (m_X:  0.864188; m_Y:  0.442863; m_Z:  0.238856),
        (m_X:  0.688191; m_Y:  0.587785; m_Z:  0.425325),
        (m_X:  0.809017; m_Y:  0.309017; m_Z:  0.500000),
        (m_X:  0.681718; m_Y:  0.147621; m_Z:  0.716567),
        (m_X:  0.587785; m_Y:  0.425325; m_Z:  0.688191),
        (m_X:  0.955423; m_Y:  0.295242; m_Z:  0.000000),
        (m_X:  1.000000; m_Y:  0.000000; m_Z:  0.000000),
        (m_X:  0.951056; m_Y:  0.162460; m_Z:  0.262866),
        (m_X:  0.850651; m_Y: -0.525731; m_Z:  0.000000),
        (m_X:  0.955423; m_Y: -0.295242; m_Z:  0.000000),
        (m_X:  0.864188; m_Y: -0.442863; m_Z:  0.238856),
        (m_X:  0.951056; m_Y: -0.162460; m_Z:  0.262866),
        (m_X:  0.809017; m_Y: -0.309017; m_Z:  0.500000),
        (m_X:  0.681718; m_Y: -0.147621; m_Z:  0.716567),
        (m_X:  0.850651; m_Y:  0.000000; m_Z:  0.525731),
        (m_X:  0.864188; m_Y:  0.442863; m_Z: -0.238856),
        (m_X:  0.809017; m_Y:  0.309017; m_Z: -0.500000),
        (m_X:  0.951056; m_Y:  0.162460; m_Z: -0.262866),
        (m_X:  0.525731; m_Y:  0.000000; m_Z: -0.850651),
        (m_X:  0.681718; m_Y:  0.147621; m_Z: -0.716567),
        (m_X:  0.681718; m_Y: -0.147621; m_Z: -0.716567),
        (m_X:  0.850651; m_Y:  0.000000; m_Z: -0.525731),
        (m_X:  0.809017; m_Y: -0.309017; m_Z: -0.500000),
        (m_X:  0.864188; m_Y: -0.442863; m_Z: -0.238856),
        (m_X:  0.951056; m_Y: -0.162460; m_Z: -0.262866),
        (m_X:  0.147621; m_Y:  0.716567; m_Z: -0.681718),
        (m_X:  0.309017; m_Y:  0.500000; m_Z: -0.809017),
        (m_X:  0.425325; m_Y:  0.688191; m_Z: -0.587785),
        (m_X:  0.442863; m_Y:  0.238856; m_Z: -0.864188),
        (m_X:  0.587785; m_Y:  0.425325; m_Z: -0.688191),
        (m_X:  0.688191; m_Y:  0.587785; m_Z: -0.425325),
        (m_X: -0.147621; m_Y:  0.716567; m_Z: -0.681718),
        (m_X: -0.309017; m_Y:  0.500000; m_Z: -0.809017),
        (m_X:  0.000000; m_Y:  0.525731; m_Z: -0.850651),
        (m_X: -0.525731; m_Y:  0.000000; m_Z: -0.850651),
        (m_X: -0.442863; m_Y:  0.238856; m_Z: -0.864188),
        (m_X: -0.295242; m_Y:  0.000000; m_Z: -0.955423),
        (m_X: -0.162460; m_Y:  0.262866; m_Z: -0.951056),
        (m_X:  0.000000; m_Y:  0.000000; m_Z: -1.000000),
        (m_X:  0.295242; m_Y:  0.000000; m_Z: -0.955423),
        (m_X:  0.162460; m_Y:  0.262866; m_Z: -0.951056),
        (m_X: -0.442863; m_Y: -0.238856; m_Z: -0.864188),
        (m_X: -0.309017; m_Y: -0.500000; m_Z: -0.809017),
        (m_X: -0.162460; m_Y: -0.262866; m_Z: -0.951056),
        (m_X:  0.000000; m_Y: -0.850651; m_Z: -0.525731),
        (m_X: -0.147621; m_Y: -0.716567; m_Z: -0.681718),
        (m_X:  0.147621; m_Y: -0.716567; m_Z: -0.681718),
        (m_X:  0.000000; m_Y: -0.525731; m_Z: -0.850651),
        (m_X:  0.309017; m_Y: -0.500000; m_Z: -0.809017),
        (m_X:  0.442863; m_Y: -0.238856; m_Z: -0.864188),
        (m_X:  0.162460; m_Y: -0.262866; m_Z: -0.951056),
        (m_X:  0.238856; m_Y: -0.864188; m_Z: -0.442863),
        (m_X:  0.500000; m_Y: -0.809017; m_Z: -0.309017),
        (m_X:  0.425325; m_Y: -0.688191; m_Z: -0.587785),
        (m_X:  0.716567; m_Y: -0.681718; m_Z: -0.147621),
        (m_X:  0.688191; m_Y: -0.587785; m_Z: -0.425325),
        (m_X:  0.587785; m_Y: -0.425325; m_Z: -0.688191),
        (m_X:  0.000000; m_Y: -0.955423; m_Z: -0.295242),
        (m_X:  0.000000; m_Y: -1.000000; m_Z:  0.000000),
        (m_X:  0.262866; m_Y: -0.951056; m_Z: -0.162460),
        (m_X:  0.000000; m_Y: -0.850651; m_Z:  0.525731),
        (m_X:  0.000000; m_Y: -0.955423; m_Z:  0.295242),
        (m_X:  0.238856; m_Y: -0.864188; m_Z:  0.442863),
        (m_X:  0.262866; m_Y: -0.951056; m_Z:  0.162460),
        (m_X:  0.500000; m_Y: -0.809017; m_Z:  0.309017),
        (m_X:  0.716567; m_Y: -0.681718; m_Z:  0.147621),
        (m_X:  0.525731; m_Y: -0.850651; m_Z:  0.000000),
        (m_X: -0.238856; m_Y: -0.864188; m_Z: -0.442863),
        (m_X: -0.500000; m_Y: -0.809017; m_Z: -0.309017),
        (m_X: -0.262866; m_Y: -0.951056; m_Z: -0.162460),
        (m_X: -0.850651; m_Y: -0.525731; m_Z:  0.000000),
        (m_X: -0.716567; m_Y: -0.681718; m_Z: -0.147621),
        (m_X: -0.716567; m_Y: -0.681718; m_Z:  0.147621),
        (m_X: -0.525731; m_Y: -0.850651; m_Z:  0.000000),
        (m_X: -0.500000; m_Y: -0.809017; m_Z:  0.309017),
        (m_X: -0.238856; m_Y: -0.864188; m_Z:  0.442863),
        (m_X: -0.262866; m_Y: -0.951056; m_Z:  0.162460),
        (m_X: -0.864188; m_Y: -0.442863; m_Z:  0.238856),
        (m_X: -0.809017; m_Y: -0.309017; m_Z:  0.500000),
        (m_X: -0.688191; m_Y: -0.587785; m_Z:  0.425325),
        (m_X: -0.681718; m_Y: -0.147621; m_Z:  0.716567),
        (m_X: -0.442863; m_Y: -0.238856; m_Z:  0.864188),
        (m_X: -0.587785; m_Y: -0.425325; m_Z:  0.688191),
        (m_X: -0.309017; m_Y: -0.500000; m_Z:  0.809017),
        (m_X: -0.147621; m_Y: -0.716567; m_Z:  0.681718),
        (m_X: -0.425325; m_Y: -0.688191; m_Z:  0.587785),
        (m_X: -0.162460; m_Y: -0.262866; m_Z:  0.951056),
        (m_X:  0.442863; m_Y: -0.238856; m_Z:  0.864188),
        (m_X:  0.162460; m_Y: -0.262866; m_Z:  0.951056),
        (m_X:  0.309017; m_Y: -0.500000; m_Z:  0.809017),
        (m_X:  0.147621; m_Y: -0.716567; m_Z:  0.681718),
        (m_X:  0.000000; m_Y: -0.525731; m_Z:  0.850651),
        (m_X:  0.425325; m_Y: -0.688191; m_Z:  0.587785),
        (m_X:  0.587785; m_Y: -0.425325; m_Z:  0.688191),
        (m_X:  0.688191; m_Y: -0.587785; m_Z:  0.425325),
        (m_X: -0.955423; m_Y:  0.295242; m_Z:  0.000000),
        (m_X: -0.951056; m_Y:  0.162460; m_Z:  0.262866),
        (m_X: -1.000000; m_Y:  0.000000; m_Z:  0.000000),
        (m_X: -0.850651; m_Y:  0.000000; m_Z:  0.525731),
        (m_X: -0.955423; m_Y: -0.295242; m_Z:  0.000000),
        (m_X: -0.951056; m_Y: -0.162460; m_Z:  0.262866),
        (m_X: -0.864188; m_Y:  0.442863; m_Z: -0.238856),
        (m_X: -0.951056; m_Y:  0.162460; m_Z: -0.262866),
        (m_X: -0.809017; m_Y:  0.309017; m_Z: -0.500000),
        (m_X: -0.864188; m_Y: -0.442863; m_Z: -0.238856),
        (m_X: -0.951056; m_Y: -0.162460; m_Z: -0.262866),
        (m_X: -0.809017; m_Y: -0.309017; m_Z: -0.500000),
        (m_X: -0.681718; m_Y:  0.147621; m_Z: -0.716567),
        (m_X: -0.681718; m_Y: -0.147621; m_Z: -0.716567),
        (m_X: -0.850651; m_Y:  0.000000; m_Z: -0.525731),
        (m_X: -0.688191; m_Y:  0.587785; m_Z: -0.425325),
        (m_X: -0.587785; m_Y:  0.425325; m_Z: -0.688191),
        (m_X: -0.425325; m_Y:  0.688191; m_Z: -0.587785),
        (m_X: -0.425325; m_Y: -0.688191; m_Z: -0.587785),
        (m_X: -0.587785; m_Y: -0.425325; m_Z: -0.688191),
        (m_X: -0.688191; m_Y: -0.587785; m_Z: -0.425325)
    );

var
    arraySize, i: Integer;

begin
    arraySize := Length(normals);
    SetLength(m_Normals, arraySize);

    // initialize normal table
    for i := 0 to arraySize - 1 do
    begin
        m_Normals[i].X := normals[i].m_X;
        m_Normals[i].Y := normals[i].m_Y;
        m_Normals[i].Z := normals[i].m_Z;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLModel.UncompressVertex(const header: TQRMDLHeader;
                                      const vertex: TQRMDLVertex): TQRVector3D;
var
    vertArray: array[0..2] of Single;
    i:         Byte;
begin
    // iterate through vertex coordinates
    for i := 0 to 2 do
        // uncompress vertex using frame scale and translate values
        vertArray[i] := (header.m_Scale[i] * vertex.m_Vertex[i]) + header.m_Translate[i];

    Result := TQRVector3D.Create(vertArray[0], vertArray[1], vertArray[2]);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLModel.GetPreCalculatedLight: TQRDirectionalLight;
begin
    Result := m_pPreCalculatedLight;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLModel.SetPreCalculatedLight(const pLight: TQRDirectionalLight);
begin
    m_pPreCalculatedLight.Assign(pLight);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLModel.GetRHToLH: Boolean;
begin
    Result := m_RHToLH;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLModel.SetRHToLH(value: Boolean);
begin
    m_RHToLH := value;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLModel.GetParser: TQRMDLParser;
begin
    Result := m_pParser;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLModel.Load(const fileName: TFileName): Boolean;
begin
    Result := m_pParser.Load(fileName);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLModel.Load(const pBuffer: TStream; readLength: NativeUInt): Boolean;
begin
    Result := m_pParser.Load(pBuffer, readLength);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLModel.GetMesh(index: NativeUInt;
                          out mesh: TQRMesh;
                         pAABBTree: TQRAABBTree;
                       hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    srcFrame:                             TQRMDLFrameGroup;
    stride, normalCount, meshIndex, i, j: NativeInt;
    offset:                               NativeUInt;
    k:                                    TQRUInt8;
    srcVertex:                            TQRMDLVertex;
    ucpVertex, normal:                    TQRVector3D;
    tu, tv:                               Single;
    pMeshColor:                           TQRColor;
    foundNormal, doFreeColor:             Boolean;
begin
    // is frame index out of bounds?
    if (index >= GetMeshCount) then
        Exit(False);

    // get normal count
    normalCount := Length(m_Normals);

    // do use normals and pre-calculated normals table wasn't populated?
    if ((EQR_VF_Normals in VertexFormat) and (normalCount = 0)) then
        Exit(False);

    // get source frame from which mesh should be extracted
    srcFrame := m_pParser.m_Frames[index];

    // basically stride is the coordinates values size
    stride := 3;

    // do include m_Normals?
    if (EQR_VF_Normals in VertexFormat) then
        Inc(stride, 3);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in VertexFormat) then
        Inc(stride, 2);

    // do include colors?
    if (EQR_VF_Colors in VertexFormat) then
        Inc(stride, 4);

    // iterate through meshes composing the frame
    for i := 0 to srcFrame.m_Count - 1 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        // add mesh to output
        SetLength(mesh, Length(mesh) + 1);
        meshIndex := Length(mesh) - 1;

        // create and populate new vertex
        mesh[meshIndex].m_Name      := 'qr_mdl';
        mesh[meshIndex].m_Stride    := stride;
        mesh[meshIndex].m_Format    := VertexFormat;
        mesh[meshIndex].m_CoordType := EQR_VC_XYZ;
        mesh[meshIndex].m_Type      := EQR_VT_Triangles;

        offset := 0;

        // iterate through polygons to process
        for j := 0 to m_pParser.m_Header.m_PolygonCount - 1 do
            for k := 0 to 2 do
            begin
                // is canceled?
                if (Assigned(hIsCanceled) and hIsCanceled) then
                    Exit(False);

                // get source vertex
                srcVertex := srcFrame.m_Frames[i].m_Vertices[m_pParser.m_Polygons[j].m_VertexIndex[k]];

                // uncompress vertex
                ucpVertex := UncompressVertex(m_pParser.m_Header, srcVertex);

                // do convert right hand <-> left hand coordinate system?
                if (m_RHToLH) then
                    // apply conversion
                    ucpVertex.X := -ucpVertex.X;

                // populate vertex buffer
                SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 3);
                mesh[meshIndex].m_Buffer[offset]     := ucpVertex.X;
                mesh[meshIndex].m_Buffer[offset + 1] := ucpVertex.Y;
                mesh[meshIndex].m_Buffer[offset + 2] := ucpVertex.Z;
                Inc(offset, 3);

                // do include m_Normals?
                if (EQR_VF_Normals in VertexFormat) then
                begin
                    // is normal index out of bounds?
                    if (srcVertex.m_NormalIndex >= normalCount) then
                        Exit(False);

                    // get vertex normal
                    normal := m_Normals[srcVertex.m_NormalIndex];

                    // do convert right hand <-> left hand coordinate system?
                    if (m_RHToLH) then
                        // apply conversion
                        normal.X := -normal.X;

                    SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 3);
                    mesh[meshIndex].m_Buffer[offset]     := normal.X;
                    mesh[meshIndex].m_Buffer[offset + 1] := normal.Y;
                    mesh[meshIndex].m_Buffer[offset + 2] := normal.Z;
                    Inc(offset, 3);
                end;

                // do include texture coordinates?
                if (EQR_VF_TexCoords in VertexFormat) then
                begin
                    // get vertex texture coordinates. Be careful, here a pointer of
                    // type float should be read from memory, for that the conversion
                    // cannot be done from M_Precision
                    tu := m_pParser.m_TexCoords[m_pParser.m_Polygons[j].m_VertexIndex[k]].m_U;
                    tv := m_pParser.m_TexCoords[m_pParser.m_Polygons[j].m_VertexIndex[k]].m_V;

                    // is texture coordinate on the back face?
                    if ((m_pParser.m_Polygons[j].m_FacesFront = 0) and
                        (m_pParser.m_TexCoords[m_pParser.m_Polygons[j].m_VertexIndex[k]].m_OnSeam <> 0))
                    then
                        // correct the texture coordinate to put it on the back face
                        tu := tu + m_pParser.m_Header.m_SkinWidth * 0.5;

                    // scale s and t to range from 0.0 to 1.0
                    tu := (tu + 0.5) / m_pParser.m_Header.m_SkinWidth;
                    tv := (tv + 0.5) / m_pParser.m_Header.m_SkinHeight;

                    SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 2);
                    mesh[meshIndex].m_Buffer[offset]     := tu;
                    mesh[meshIndex].m_Buffer[offset + 1] := tv;
                    Inc(offset, 2);
                end;

                // do include colors?
                if (EQR_VF_Colors in VertexFormat) then
                begin
                    doFreeColor := False;

                    // do pre-calculate lightining or use the material color?
                    if (m_pPreCalculatedLight.Enabled) then
                    begin
                        // by default, normal exists
                        foundNormal := True;

                        // are normals also included?
                        if (not(EQR_VF_Normals in VertexFormat)) then
                        begin
                            // is normal index out of bounds?
                            if (srcVertex.m_NormalIndex >= normalCount) then
                            begin
                                // normal isn't available
                                foundNormal := False;
                            end
                            else
                            begin
                                // get vertex normal
                                normal := m_Normals[srcVertex.m_NormalIndex];

                                // do convert right hand <-> left hand coordinate system?
                                if (m_RHToLH) then
                                    // apply conversion
                                    normal.X := -normal.X;
                            end;
                        end;

                        // calculate lightning from pre-calculated light
                        if (foundNormal) then
                        begin
                            doFreeColor := True;
                            pMeshColor  := CalculateLight(normal, m_pPreCalculatedLight);
                        end
                        else
                            // unfortunately normal isn't available, so use the ambient color (not the
                            // best solution, but for lack of better...)
                            pMeshColor := m_pPreCalculatedLight.Ambient;
                    end
                    else
                    begin
                        // use default color
                        pMeshColor := Color;
                    end;

                    SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 4);
                    mesh[meshIndex].m_Buffer[offset]     := pMeshColor.GetRedF;
                    mesh[meshIndex].m_Buffer[offset + 1] := pMeshColor.GetGreenF;
                    mesh[meshIndex].m_Buffer[offset + 2] := pMeshColor.GetBlueF;
                    mesh[meshIndex].m_Buffer[offset + 3] := pMeshColor.GetAlphaF;
                    Inc(offset, 4);

                    if (doFreeColor) then
                        pMeshColor.Free;
                end;
            end;
    end;

    // canceled?
    if (Assigned(hIsCanceled) and hIsCanceled) then
        Exit(True);

    // no aligned-axis bounding box tree to populate?
    if (not Assigned(pAABBTree)) then
        Exit(True);

    // populate aligned-axis bounding box tree
    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLModel.GetMesh(index, nextIndex: NativeUInt;
                          interpolationFactor: Double;
                                     out mesh: TQRMesh;
                                  hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    srcFrame, intFrame:                                                   TQRMDLFrameGroup;
    stride, normalCount, i, j, meshIndex:                                 NativeInt;
    offset:                                                               NativeUInt;
    k:                                                                    Byte;
    srcVertex, intVertex:                                                 TQRMDLVertex;
    ucpVertex, ucpIntVertex, finalVertex, normal, intNormal, finalNormal: TQRVector3D;
    tu, tv:                                                               Single;
    pMeshColor:                                                           TQRColor;
    foundNormal, doFreeColor:                                             Boolean;
begin
    // is frame index out of bounds?
    if (index >= GetMeshCount) then
        Exit(False);

    // get normal count
    normalCount := Length(m_Normals);

    // do use normals and pre-calculated normals table wasn't populated?
    if ((EQR_VF_Normals in VertexFormat) and (normalCount = 0)) then
        Exit(False);

    // get source frame from which mesh should be extracted, and frame to interpolate with
    srcFrame := m_pParser.m_Frames[index];
    intFrame := m_pParser.m_Frames[nextIndex];

    // basically stride is the coordinates values size
    stride := 3;

    // do include m_Normals?
    if (EQR_VF_Normals in VertexFormat) then
        Inc(stride, 3);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in VertexFormat) then
        Inc(stride, 2);

    // do include colors?
    if (EQR_VF_Colors in VertexFormat) then
        Inc(stride, 4);

    // iterate through meshes composing the frame
    for i := 0 to srcFrame.m_Count - 1 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        // add mesh to output
        SetLength(mesh, Length(mesh) + 1);
        meshIndex := Length(mesh) - 1;

        // create and populate new vertex
        mesh[meshIndex].m_Name      := 'qr_mdl';
        mesh[meshIndex].m_Stride    := stride;
        mesh[meshIndex].m_Format    := VertexFormat;
        mesh[meshIndex].m_CoordType := EQR_VC_XYZ;
        mesh[meshIndex].m_Type      := EQR_VT_Triangles;

        offset := 0;

        // iterate through polygons to process
        for j := 0 to m_pParser.m_Header.m_PolygonCount - 1 do
            for k := 0 to 2 do
            begin
                // is canceled?
                if (Assigned(hIsCanceled) and hIsCanceled) then
                    Exit(False);

                // get source vertex, and vertex to interpolate with
                srcVertex := srcFrame.m_Frames[i].m_Vertices[m_pParser.m_Polygons[j].m_VertexIndex[k]];
                intVertex := intFrame.m_Frames[i].m_Vertices[m_pParser.m_Polygons[j].m_VertexIndex[k]];

                // uncompress vertices
                ucpVertex    := UncompressVertex(m_pParser.m_Header, srcVertex);
                ucpIntVertex := UncompressVertex(m_pParser.m_Header, intVertex);

                // do convert right hand <-> left hand coordinate system?
                if (m_RHToLH) then
                begin
                    // apply conversion
                    ucpVertex.X    := -ucpVertex.X;
                    ucpIntVertex.X := -ucpIntVertex.X;
                end;

                // calculate final mesh vertex
                finalVertex := ucpVertex.Interpolate(ucpIntVertex, interpolationFactor);

                // populate vertex buffer
                SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 3);
                mesh[meshIndex].m_Buffer[offset]     := finalVertex.X;
                mesh[meshIndex].m_Buffer[offset + 1] := finalVertex.Y;
                mesh[meshIndex].m_Buffer[offset + 2] := finalVertex.Z;
                Inc(offset, 3);

                // do include m_Normals?
                if (EQR_VF_Normals in VertexFormat) then
                begin
                    // is normal index out of bounds?
                    if (srcVertex.m_NormalIndex >= normalCount) then
                        Exit(False);

                    // get vertex normal, and normal to interpolate with
                    normal    := m_Normals[srcVertex.m_NormalIndex];
                    intNormal := m_Normals[intVertex.m_NormalIndex];

                    // do convert right hand <-> left hand coordinate system?
                    if (m_RHToLH) then
                    begin
                        // apply conversion
                        normal.X    := -normal.X;
                        intNormal.X := -intNormal.X;
                    end;

                    // calculate final vertex normal
                    finalNormal := normal.Interpolate(intNormal, interpolationFactor);

                    SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 3);
                    mesh[meshIndex].m_Buffer[offset]     := finalNormal.X;
                    mesh[meshIndex].m_Buffer[offset + 1] := finalNormal.Y;
                    mesh[meshIndex].m_Buffer[offset + 2] := finalNormal.Z;
                    Inc(offset, 3);
                end;

                // do include texture coordinates?
                if (EQR_VF_TexCoords in VertexFormat) then
                begin
                    // get vertex texture coordinates. Be careful, here a pointer of
                    // type float should be read from memory, for that the conversion
                    // cannot be done from M_Precision
                    tu := m_pParser.m_TexCoords[m_pParser.m_Polygons[j].m_VertexIndex[k]].m_U;
                    tv := m_pParser.m_TexCoords[m_pParser.m_Polygons[j].m_VertexIndex[k]].m_V;

                    // is texture coordinate on the back face?
                    if ((m_pParser.m_Polygons[j].m_FacesFront = 0) and
                        (m_pParser.m_TexCoords[m_pParser.m_Polygons[j].m_VertexIndex[k]].m_OnSeam <> 0))
                    then
                        // correct the texture coordinate to put it on the back face
                        tu := tu + m_pParser.m_Header.m_SkinWidth * 0.5;

                    // scale s and t to range from 0.0 to 1.0
                    tu := (tu + 0.5) / m_pParser.m_Header.m_SkinWidth;
                    tv := (tv + 0.5) / m_pParser.m_Header.m_SkinHeight;

                    SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 2);
                    mesh[meshIndex].m_Buffer[offset]     := tu;
                    mesh[meshIndex].m_Buffer[offset + 1] := tv;
                    Inc(offset, 2);
                end;

                // do include colors?
                if (EQR_VF_Colors in VertexFormat) then
                begin
                    doFreeColor := False;

                    // do pre-calculate lightining or use the material color?
                    if (m_pPreCalculatedLight.Enabled) then
                    begin
                        // by default, normal exists
                        foundNormal := True;

                        // are normals also included?
                        if (not(EQR_VF_Normals in VertexFormat)) then
                        begin
                            // is normal index out of bounds?
                            if (srcVertex.m_NormalIndex >= normalCount) then
                            begin
                                // normal isn't available
                                foundNormal := False;
                            end
                            else
                            begin
                                // get vertex normal, and normal to interpolate with
                                normal    := m_Normals[srcVertex.m_NormalIndex];
                                intNormal := m_Normals[intVertex.m_NormalIndex];

                                // do convert right hand <-> left hand coordinate system?
                                if (m_RHToLH) then
                                begin
                                    // apply conversion
                                    normal.X    := -normal.X;
                                    intNormal.X := -intNormal.X;
                                end;

                                // calculate final vertex normal
                                finalNormal := normal.Interpolate(intNormal, interpolationFactor);
                            end;
                        end;

                        // calculate lightning from pre-calculated light
                        if (foundNormal) then
                        begin
                            doFreeColor := True;
                            pMeshColor  := CalculateLight(finalNormal, m_pPreCalculatedLight);
                        end
                        else
                            // unfortunately normal isn't available, so use the ambient color (not the
                            // best solution, but for lack of better...)
                            pMeshColor := m_pPreCalculatedLight.Ambient;
                    end
                    else
                    begin
                        // use default color
                        pMeshColor := Color;
                    end;

                    SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 4);
                    mesh[meshIndex].m_Buffer[offset]     := pMeshColor.GetRedF;
                    mesh[meshIndex].m_Buffer[offset + 1] := pMeshColor.GetGreenF;
                    mesh[meshIndex].m_Buffer[offset + 2] := pMeshColor.GetBlueF;
                    mesh[meshIndex].m_Buffer[offset + 3] := pMeshColor.GetAlphaF;
                    Inc(offset, 4);

                    if (doFreeColor) then
                        pMeshColor.Free;
                end;
            end;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLModel.GetMeshCount: NativeUInt;
begin
    Result := m_pParser.m_Header.m_FrameCount;
end;
//--------------------------------------------------------------------------------------------------

end.
