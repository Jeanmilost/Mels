// *************************************************************************************************
// * ==> UTQRShapes -------------------------------------------------------------------------------*
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
 @abstract(@name provides the features to create models based on shapes, as e.g. a sphere, a cube or
                 a pyramid, and build their vertex buffer.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRShapes;

{$MODE Delphi}

interface

uses Math,
     UTQRCommon,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRModel;

type
    {$REGION 'Documentation'}
    {**
     Shape helper
    }
    {$ENDREGION}
    TQRShapeHelper = class
        public
            {$REGION 'Documentation'}
            {**
             Calculates normal from points
             @param(pPt1 First point)
             @param(pPt2 Second point)
             @param(pPt3 Third point)
             @return(Normal)
             @br @bold(NOTE) Normal is calculated as follow:
             @longcode(#
                          normal
                             |
                             |
                             |
                        pPt1 |__________ pPt2
                            /
                           /
                          /
                       pPt3
                       #)
            }
            {$ENDREGION}
            class function NormalFromPoints(const pPt1, pPt2, pPt3: PQRVector3D): TQRVector3D; static;

            {$REGION 'Documentation'}
            {**
             Adds vertex to a vertex buffer
             @param(pPosition Vertex possition in space 3D coordinates)
             @param(pNormal Vertex normal)
             @param(pTexCoord Vertex texture coordinates)
             @param(pColor Vertex color)
             @param(index @bold([in, out]) Vertex index in the buffer)
             @param(vertex @bold([in, out]) Vertex info containing buffer in which vertex should be added)
            }
            {$ENDREGION}
            class procedure AddVertex(const pPosition: PQRVector3D;
                                        const pNormal: PQRVector3D;
                                      const pTexCoord: PQRVector2D;
                                         const pColor: TQRColor;
                                            var index: NativeUInt;
                                           var vertex: TQRVertex); static;
    end;

    {$REGION 'Documentation'}
    {**
     Generic 3D shape
    }
    {$ENDREGION}
    TQRShapeModel = class(TQRStaticModel)
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
    end;

    {$REGION 'Documentation'}
    {**
     3D surface
     @br @bold(NOTE) Position is measured from the center of the shape
    }
    {$ENDREGION}
    TQRSurfaceModel = class(TQRShapeModel)
        private
            m_LengthX: Single;
            m_LengthY: Single;

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
             Gets the mesh
             @param(mesh @bold([out]) Mesh)
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
            function GetMesh(out mesh: TQRMesh;
                            pAABBTree: TQRAABBTree;
                          hIsCanceled: TQRIsCanceledEvent = nil): Boolean; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the surface length on the x axis
             @br @br
             @image(Resources/Images/Documentation/Surface width.svg)
            }
            {$ENDREGION}
            property LengthX: Single read m_LengthX write m_LengthX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the surface length on the y axis
             @br @br
             @image(Resources/Images/Documentation/Surface height.svg)
            }
            {$ENDREGION}
            property LengthY: Single read m_LengthY write m_LengthY;
    end;

    {$REGION 'Documentation'}
    {**
     3D box
     @br @bold(NOTE) This shape may also be used to build a cube
     @br @bold(NOTE) Position is measured from the center of the shape
    }
    {$ENDREGION}
    TQRBoxModel = class(TQRShapeModel)
        private
            m_LengthX:             Single;
            m_LengthY:             Single;
            m_LengthZ:             Single;
            m_RepeatTexOnEachFace: Boolean;

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
             Gets the mesh
             @param(mesh @bold([out]) Mesh)
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
            function GetMesh(out mesh: TQRMesh;
                            pAABBTree: TQRAABBTree;
                          hIsCanceled: TQRIsCanceledEvent = nil): Boolean; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the box length on the x axis
             @br @br
             @image(Resources/Images/Documentation/Cube width.svg)
            }
            {$ENDREGION}
            property LengthX: Single  read m_LengthX write m_LengthX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the box length on the y axis
             @br @br
             @image(Resources/Images/Documentation/Cube height.svg)
            }
            {$ENDREGION}
            property LengthY: Single  read m_LengthY write m_LengthY;

            {$REGION 'Documentation'}
            {**
             Gets or sets the box length on the z axis
             @br @br
             @image(Resources/Images/Documentation/Cube depth.svg)
            }
            {$ENDREGION}
            property LengthZ: Single  read m_LengthZ write m_LengthZ;

            {$REGION 'Documentation'}
            {**
             Gets or sets if the texture should be repeated on each face of the box (@true), or if
             the texture should wrap the entire box (@false)
             @br @bold(NOTE) A part of the texture surface will be wasted if the texture wraps the
                             entire box, a shown on the following scheme:
             @longcode(#
                       ----------------------------
                       |        |        |        |
                       | Face 1 | Face 2 | Face 3 |
                       |        |        |        |
                       ----------------------------
                       |        |        |        |
                       | Face 4 | Face 5 | Face 6 |
                       |        |        |        |
                       ----------------------------
                       |////////|////////|////////|
                       |/Wasted/|/Wasted/|/Wasted/|
                       |////////|////////|////////|
                       ----------------------------
                       #)
            }
            {$ENDREGION}
            property RepeatTexOnEachFace: Boolean read m_RepeatTexOnEachFace write m_RepeatTexOnEachFace;
    end;

    {$REGION 'Documentation'}
    {**
     3D sphere
     @br @bold(NOTE) Position is measured from the center of the shape
    }
    {$ENDREGION}
    TQRSphereModel = class(TQRShapeModel)
        private
            m_Slices: NativeUInt;
            m_Stacks: NativeUInt;
            m_Radius: Single;

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
             Gets the mesh
             @param(mesh @bold([out]) Mesh)
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
            function GetMesh(out mesh: TQRMesh;
                            pAABBTree: TQRAABBTree;
                          hIsCanceled: TQRIsCanceledEvent = nil): Boolean; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the slices count that will be used to divide the sphere while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Sphere Slices.svg)
            }
            {$ENDREGION}
            property Slices: NativeUInt read m_Slices write m_Slices;

            {$REGION 'Documentation'}
            {**
             Gets or sets the stacks count that will be used to divide the sphere while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Sphere Stacks.svg)
            }
            {$ENDREGION}
            property Stacks: NativeUInt read m_Stacks write m_Stacks;

            {$REGION 'Documentation'}
            {**
             Gets or sets the sphere radius
             @br @br
             @image(Resources/Images/Documentation/Sphere Radius.svg)
            }
            {$ENDREGION}
            property Radius: Single read m_Radius write m_Radius;
    end;

    {$REGION 'Documentation'}
    {**
     Cone closing type
     @value(EQR_CC_None Indicates that the cone is opened on the both top and bottom)
     @value(EQR_CC_Top Indicates that the cone is closed on the top and opened on the bottom)
     @value(EQR_CC_Bottom Indicates that the cone is opened on the top and closed on the bottom)
     @value(EQR_CC_Both Indicates that the cone is closed on the both top and bottom)
    }
    {$ENDREGION}
    EQR_Cone_Closing =
    (
        EQR_CC_None = 0,
        EQR_CC_Top,
        EQR_CC_Bottom,
        EQR_CC_Both
    );

    {$REGION 'Documentation'}
    {**
     3D cone
     @br @bold(NOTE) This shape may also be used to build a truncated cone (where top radius is
                     higher than 0), a pyramid (a cone with 3 or 4 faces), a cylinder (a truncated
                     cone where top and bottom radius are equals), or a diabolo (a truncated cone
                     where either top or bottom radius is negative)
     @br @bold(NOTE) Position is measured from the center of the base
    }
    {$ENDREGION}
    TQRConeModel = class(TQRShapeModel)
        private
            m_FaceCount:     NativeUInt;
            m_Height:        Single;
            m_TopRadiusX:    Single;
            m_TopRadiusY:    Single;
            m_BottomRadiusX: Single;
            m_BottomRadiusY: Single;
            m_Closing:       EQR_Cone_Closing;

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
             Gets the mesh
             @param(mesh @bold([out]) Mesh)
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
            function GetMesh(out mesh: TQRMesh;
                            pAABBTree: TQRAABBTree;
                          hIsCanceled: TQRIsCanceledEvent = nil): Boolean; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the number of faces to generate while cone vertex buffer will be created
             @br @br
             @image(Resources/Images/Documentation/Cone Face Count.svg)
            }
            {$ENDREGION}
            property FaceCount: NativeUInt read m_FaceCount write m_FaceCount;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone height
             @br @br
             @image(Resources/Images/Documentation/Cone Height.svg)
            }
            {$ENDREGION}
            property Height: Single read m_Height write m_Height;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone top radius on the x axis
             @br @br
             @image(Resources/Images/Documentation/Cone Radius Top X.svg)
            }
            {$ENDREGION}
            property TopRadiusX: Single read m_TopRadiusX write m_TopRadiusX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone top radius on the y axis
             @br @br
             @image(Resources/Images/Documentation/Cone Radius Top Y.svg)
            }
            {$ENDREGION}
            property TopRadiusY: Single read m_TopRadiusY write m_TopRadiusY;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone bottom radius on the x axis
             @br @br
             @image(Resources/Images/Documentation/Cone Radius Bottom X.svg)
            }
            {$ENDREGION}
            property BottomRadiusX: Single read m_BottomRadiusX write m_BottomRadiusX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone bottom radius on the y axis
             @br @br
             @image(Resources/Images/Documentation/Cone Radius Bottom Y.svg)
            }
            {$ENDREGION}
            property BottomRadiusY: Single read m_BottomRadiusY write m_BottomRadiusY;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone closing mode
            }
            {$ENDREGION}
            property Closing: EQR_Cone_Closing read m_Closing write m_Closing;
    end;

    {$REGION 'Documentation'}
    {**
     3D torus
     @br @bold(NOTE) Position is measured from the center of the shape
    }
    {$ENDREGION}
    TQRTorusModel = class(TQRShapeModel)
        private
            m_Slices:         NativeUInt;
            m_FacesPerSlices: NativeUInt;
            m_OuterRadiusX:   Single;
            m_OuterRadiusY:   Single;
            m_InnerRadiusX:   Single;
            m_InnerRadiusY:   Single;

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
             Gets the mesh
             @param(mesh @bold([out]) Mesh)
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
            function GetMesh(out mesh: TQRMesh;
                            pAABBTree: TQRAABBTree;
                          hIsCanceled: TQRIsCanceledEvent = nil): Boolean; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the slices count that will be used to divide the torus while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Torus Slices.svg)
            }
            {$ENDREGION}
            property Slices: NativeUInt read m_Slices write m_Slices;

            {$REGION 'Documentation'}
            {**
             Gets or sets the number of faces per slices to apply to the torus while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Torus Faces Per Slices.svg)
            }
            {$ENDREGION}
            property FacesPerSlices: NativeUInt read m_FacesPerSlices write m_FacesPerSlices;

            {$REGION 'Documentation'}
            {**
             Gets or sets the torus outer radius on the x axis
             @br @br
             @image(Resources/Images/Documentation/Torus Outer Radius X.svg)
            }
            {$ENDREGION}
            property OuterRadiusX: Single read m_OuterRadiusX write m_OuterRadiusX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the torus outer radius on the y axis
             @br @br
             @image(Resources/Images/Documentation/Torus Outer Radius Y.svg)
            }
            {$ENDREGION}
            property OuterRadiusY: Single read m_OuterRadiusY write m_OuterRadiusY;

            {$REGION 'Documentation'}
            {**
             Gets or sets the torus inner radius on the x axis
             @br @br
             @image(Resources/Images/Documentation/Torus inner Radius X.svg)
            }
            {$ENDREGION}
            property InnerRadiusX: Single read m_InnerRadiusX write m_InnerRadiusX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the torus inner radius on the y axis
             @br @br
             @image(Resources/Images/Documentation/Torus Inner Radius Y.svg)
            }
            {$ENDREGION}
            property InnerRadiusY: Single read m_InnerRadiusY write m_InnerRadiusY;
    end;

    {$REGION 'Documentation'}
    {**
     3D parabola
     @br @bold(NOTE) Position is measured from the point where the parabola climaxed. This also
                     means that the rotation axis may be eccentric
    }
    {$ENDREGION}
    TQRParabolaModel = class(TQRShapeModel)
        private
            m_Slices:         NativeUInt;
            m_FacesPerSlices: NativeUInt;
            m_Height:         Single;
            m_Radius:         Single;

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
             Gets the mesh
             @param(mesh @bold([out]) Mesh)
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
            function GetMesh(out mesh: TQRMesh;
                            pAABBTree: TQRAABBTree;
                          hIsCanceled: TQRIsCanceledEvent = nil): Boolean; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the slices count that will be used to divide the parabola while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Parabola Slices.svg)
            }
            {$ENDREGION}
            property Slices: NativeUInt read m_Slices write m_Slices;

            {$REGION 'Documentation'}
            {**
             Gets or sets the number of faces per slices to apply to the parabola while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Parabola Faces Per Slices.svg)
            }
            {$ENDREGION}
            property FacesPerSlices: NativeUInt read m_FacesPerSlices write m_FacesPerSlices;

            {$REGION 'Documentation'}
            {**
             Gets or sets the parabola height
             @br @br
             @image(Resources/Images/Documentation/Parabola Height.svg)
            }
            {$ENDREGION}
            property Height: Single read m_Height write m_Height;

            {$REGION 'Documentation'}
            {**
             Gets or sets the parabola radius
             @br @br
             @image(Resources/Images/Documentation/Parabola Radius.svg)
            }
            {$ENDREGION}
            property Radius: Single read m_Radius write m_Radius;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRShapeHelper
//--------------------------------------------------------------------------------------------------
class function TQRShapeHelper.NormalFromPoints(const pPt1, pPt2, pPt3: PQRVector3D): TQRVector3D;
begin
    Result := pPt2.Sub(pPt1^).Cross(pPt3.Sub(pPt1^)).Normalize;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRShapeHelper.AddVertex(const pPosition: PQRVector3D;
                                           const pNormal: PQRVector3D;
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
//--------------------------------------------------------------------------------------------------
// TQRShapeModel
//--------------------------------------------------------------------------------------------------
constructor TQRShapeModel.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRShapeModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRSurfaceModel
//--------------------------------------------------------------------------------------------------
constructor TQRSurfaceModel.Create;
begin
    inherited Create;

    m_LengthX := 0.0;
    m_LengthY := 0.0;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRSurfaceModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRSurfaceModel.GetMesh(out mesh: TQRMesh;
                                pAABBTree: TQRAABBTree;
                              hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    halfX, halfY:     Single;
    index, stride, i: NativeUInt;
    normal:           TQRVector3D;
    vertices:         array [0..3] of TQRVector3D;
    texCoords:        array [0..3] of TQRVector2D;
begin
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

    // calculate half values
    halfX := m_LengthX / 2.0;
    halfY := m_LengthY / 2.0;

    // create mesh vertices
    SetLength(mesh, 1);

    // is canceled?
    if (Assigned(hIsCanceled) and hIsCanceled) then
        Exit(False);

    mesh[0].m_Name      := 'qr_surface';
    mesh[0].m_Stride    := stride;
    mesh[0].m_Type      := EQR_VT_TriangleStrip;
    mesh[0].m_Format    := VertexFormat;
    mesh[0].m_CoordType := EQR_VC_XYZ;

    // is canceled?
    if (Assigned(hIsCanceled) and hIsCanceled) then
        Exit(False);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in VertexFormat) then
    begin
        // calculate vertices and texture coordinates
        vertices[0].X := -halfX; vertices[0].Y := -halfY; vertices[0].Z := 0.0; texCoords[0].X := 0.0; texCoords[0].Y := 1.0;
        vertices[1].X :=  halfX; vertices[1].Y := -halfY; vertices[1].Z := 0.0; texCoords[1].X := 1.0; texCoords[1].Y := 1.0;
        vertices[2].X := -halfX; vertices[2].Y :=  halfY; vertices[2].Z := 0.0; texCoords[2].X := 0.0; texCoords[2].Y := 0.0;
        vertices[3].X :=  halfX; vertices[3].Y :=  halfY; vertices[3].Z := 0.0; texCoords[3].X := 1.0; texCoords[3].Y := 0.0;
    end
    else
    begin
        // calculate vertices
        vertices[0].X := -halfX; vertices[0].Y := -halfY; vertices[0].Z := 0.0;
        vertices[1].X := -halfX; vertices[1].Y :=  halfY; vertices[1].Z := 0.0;
        vertices[2].X :=  halfX; vertices[2].Y := -halfY; vertices[2].Z := 0.0;
        vertices[3].X :=  halfX; vertices[3].Y :=  halfY; vertices[3].Z := 0.0;
    end;

    // do include normals?
    if (EQR_VF_Normals in VertexFormat) then
        // calculate normal
        normal := TQRVector3D.Create(0.0, 0.0, 1.0);

    index := 0;

    // iterate through vertices to create
    for i := 0 to 3 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        // add left bottom vertex
        TQRShapeHelper.AddVertex(@vertices[i], @normal, @texCoords[i], Color, index, mesh[0]);
    end;

    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
// TQRBoxModel
//--------------------------------------------------------------------------------------------------
constructor TQRBoxModel.Create;
begin
    inherited Create;

    m_LengthX             := 0.0;
    m_LengthY             := 0.0;
    m_LengthZ             := 0.0;
    m_RepeatTexOnEachFace := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRBoxModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRBoxModel.GetMesh(out mesh: TQRMesh;
                            pAABBTree: TQRAABBTree;
                          hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    halfX, halfY, halfZ, texOffset: Single;
    i, index, stride:               NativeUInt;
    vertices:                       array [0..7]  of TQRVector3D;
    normals:                        array [0..5]  of TQRVector3D;
    texCoords:                      array [0..23] of TQRVector2D;
begin
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

    // calculate half values
    halfX := m_LengthX / 2.0;
    halfY := m_LengthY / 2.0;
    halfZ := m_LengthZ / 2.0;

    // iterate through vertices to create. Vertices are generated as follow:
    //     v2 *--------* v6
    //      / |      / |
    // v4 *--------* v8|
    //    |   |    |   |
    //    |v1 *----|---* v5
    //    | /      | /
    // v3 *--------* v7
    for i := 0 to 7 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        // generate the 4 first vertices on the left, and 4 last on the right
        if (Floor(i div 4) = 0) then
            vertices[i].X := -halfX
        else
            vertices[i].X :=  halfX;

        // generate 2 vertices on the front, then 2 vertices on the back
        if ((Floor(i div 2) mod 2) = 0) then
            vertices[i].Z := -halfZ
        else
            vertices[i].Z :=  halfZ;

        // for each vertices, generates one on the top, and one on the bottom
        if ((i mod 2) = 0) then
            vertices[i].Y := -halfY
        else
            vertices[i].Y :=  halfY;
    end;

    // create mesh vertices
    SetLength(mesh, 6);

    // initialize each mesh
    for i := 0 to 5 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        mesh[i].m_Name      := 'qr_box';
        mesh[i].m_Stride    := stride;
        mesh[i].m_Type      := EQR_VT_TriangleStrip;
        mesh[i].m_Format    := VertexFormat;
        mesh[i].m_CoordType := EQR_VC_XYZ;
    end;

    // calculate normals
    normals[0] := TQRVector3D.Create(-1.0,  0.0,  0.0);
    normals[1] := TQRVector3D.Create( 1.0,  0.0,  0.0);
    normals[2] := TQRVector3D.Create( 0.0, -1.0,  0.0);
    normals[3] := TQRVector3D.Create( 0.0,  1.0,  0.0);
    normals[4] := TQRVector3D.Create( 0.0,  0.0, -1.0);
    normals[5] := TQRVector3D.Create( 0.0,  0.0,  1.0);

    // do repeat texture on each faces?
    if (m_RepeatTexOnEachFace) then
    begin
        // calculate texture positions
        texCoords[0].X  := 0.0; texCoords[0].Y  := 0.0;
        texCoords[1].X  := 0.0; texCoords[1].Y  := 1.0;
        texCoords[2].X  := 1.0; texCoords[2].Y  := 0.0;
        texCoords[3].X  := 1.0; texCoords[3].Y  := 1.0;
        texCoords[4].X  := 0.0; texCoords[4].Y  := 0.0;
        texCoords[5].X  := 0.0; texCoords[5].Y  := 1.0;
        texCoords[6].X  := 1.0; texCoords[6].Y  := 0.0;
        texCoords[7].X  := 1.0; texCoords[7].Y  := 1.0;
        texCoords[8].X  := 0.0; texCoords[8].Y  := 0.0;
        texCoords[9].X  := 0.0; texCoords[9].Y  := 1.0;
        texCoords[10].X := 1.0; texCoords[10].Y := 0.0;
        texCoords[11].X := 1.0; texCoords[11].Y := 1.0;
        texCoords[12].X := 0.0; texCoords[12].Y := 0.0;
        texCoords[13].X := 0.0; texCoords[13].Y := 1.0;
        texCoords[14].X := 1.0; texCoords[14].Y := 0.0;
        texCoords[15].X := 1.0; texCoords[15].Y := 1.0;
        texCoords[16].X := 0.0; texCoords[16].Y := 0.0;
        texCoords[17].X := 0.0; texCoords[17].Y := 1.0;
        texCoords[18].X := 1.0; texCoords[18].Y := 0.0;
        texCoords[19].X := 1.0; texCoords[19].Y := 1.0;
        texCoords[20].X := 0.0; texCoords[20].Y := 0.0;
        texCoords[21].X := 0.0; texCoords[21].Y := 1.0;
        texCoords[22].X := 1.0; texCoords[22].Y := 0.0;
        texCoords[23].X := 1.0; texCoords[23].Y := 1.0;
    end
    else
    begin
        // calculate texture offset
        texOffset := 1.0 / 3.0;

        // calculate texture positions. They are distributed as follow:
        // -------------------
        // |     |     |     |
        // |  1  |  2  |  3  |
        // |     |     |     |
        // |-----------------|
        // |     |     |     |
        // |  4  |  5  |  6  |
        // |     |     |     |
        // -------------------
        // |  This texture   |
        // |  area is not    |
        // |  used           |
        // -------------------
        texCoords[0].X  := 0.0;             texCoords[0].Y  := texOffset;
        texCoords[1].X  := 0.0;             texCoords[1].Y  := 0.0;
        texCoords[2].X  := texOffset;       texCoords[2].Y  := texOffset;
        texCoords[3].X  := texOffset;       texCoords[3].Y  := 0.0;
        texCoords[4].X  := texOffset;       texCoords[4].Y  := texOffset;
        texCoords[5].X  := texOffset;       texCoords[5].Y  := 0.0;
        texCoords[6].X  := texOffset * 2.0; texCoords[6].Y  := texOffset;
        texCoords[7].X  := texOffset * 2.0; texCoords[7].Y  := 0.0;
        texCoords[8].X  := texOffset * 2.0; texCoords[8].Y  := texOffset;
        texCoords[9].X  := texOffset * 2.0; texCoords[9].Y  := 0.0;
        texCoords[10].X := 1.0;             texCoords[10].Y := texOffset;
        texCoords[11].X := 1.0;             texCoords[11].Y := 0.0;
        texCoords[12].X := 0.0;             texCoords[12].Y := texOffset * 2.0;
        texCoords[13].X := 0.0;             texCoords[13].Y := texOffset;
        texCoords[14].X := texOffset;       texCoords[14].Y := texOffset * 2.0;
        texCoords[15].X := texOffset;       texCoords[15].Y := texOffset;
        texCoords[16].X := texOffset;       texCoords[16].Y := texOffset * 2.0;
        texCoords[17].X := texOffset;       texCoords[17].Y := texOffset;
        texCoords[18].X := texOffset * 2.0; texCoords[18].Y := texOffset * 2.0;
        texCoords[19].X := texOffset * 2.0; texCoords[19].Y := texOffset;
        texCoords[20].X := texOffset * 2.0; texCoords[20].Y := texOffset * 2.0;
        texCoords[21].X := texOffset * 2.0; texCoords[21].Y := texOffset;
        texCoords[22].X := 1.0;             texCoords[22].Y := texOffset * 2.0;
        texCoords[23].X := 1.0;             texCoords[23].Y := texOffset;
    end;

    // is canceled?
    if (Assigned(hIsCanceled) and hIsCanceled) then
        Exit(False);

    index := 0;

    // create box edge 1
    TQRShapeHelper.AddVertex(@vertices[1], @normals[0], @texCoords[4], Color, index, mesh[0]);
    TQRShapeHelper.AddVertex(@vertices[0], @normals[0], @texCoords[5], Color, index, mesh[0]);
    TQRShapeHelper.AddVertex(@vertices[3], @normals[0], @texCoords[6], Color, index, mesh[0]);
    TQRShapeHelper.AddVertex(@vertices[2], @normals[0], @texCoords[7], Color, index, mesh[0]);

    index := 0;

    // create box edge 2
    TQRShapeHelper.AddVertex(@vertices[3], @normals[5], @texCoords[8],  Color, index, mesh[1]);
    TQRShapeHelper.AddVertex(@vertices[2], @normals[5], @texCoords[9],  Color, index, mesh[1]);
    TQRShapeHelper.AddVertex(@vertices[7], @normals[5], @texCoords[10], Color, index, mesh[1]);
    TQRShapeHelper.AddVertex(@vertices[6], @normals[5], @texCoords[11], Color, index, mesh[1]);

    index := 0;

    // create box edge 3
    TQRShapeHelper.AddVertex(@vertices[7], @normals[1], @texCoords[12], Color, index, mesh[2]);
    TQRShapeHelper.AddVertex(@vertices[6], @normals[1], @texCoords[13], Color, index, mesh[2]);
    TQRShapeHelper.AddVertex(@vertices[5], @normals[1], @texCoords[14], Color, index, mesh[2]);
    TQRShapeHelper.AddVertex(@vertices[4], @normals[1], @texCoords[15], Color, index, mesh[2]);

    index := 0;

    // create box edge 4
    TQRShapeHelper.AddVertex(@vertices[5], @normals[4], @texCoords[16], Color, index, mesh[3]);
    TQRShapeHelper.AddVertex(@vertices[4], @normals[4], @texCoords[17], Color, index, mesh[3]);
    TQRShapeHelper.AddVertex(@vertices[1], @normals[4], @texCoords[18], Color, index, mesh[3]);
    TQRShapeHelper.AddVertex(@vertices[0], @normals[4], @texCoords[19], Color, index, mesh[3]);

    index := 0;

    // create box edge 5
    TQRShapeHelper.AddVertex(@vertices[1], @normals[3], @texCoords[0], Color, index, mesh[4]);
    TQRShapeHelper.AddVertex(@vertices[3], @normals[3], @texCoords[1], Color, index, mesh[4]);
    TQRShapeHelper.AddVertex(@vertices[5], @normals[3], @texCoords[2], Color, index, mesh[4]);
    TQRShapeHelper.AddVertex(@vertices[7], @normals[3], @texCoords[3], Color, index, mesh[4]);

    index := 0;

    // create box edge 6
    TQRShapeHelper.AddVertex(@vertices[2], @normals[2], @texCoords[20], Color, index, mesh[5]);
    TQRShapeHelper.AddVertex(@vertices[0], @normals[2], @texCoords[21], Color, index, mesh[5]);
    TQRShapeHelper.AddVertex(@vertices[6], @normals[2], @texCoords[22], Color, index, mesh[5]);
    TQRShapeHelper.AddVertex(@vertices[4], @normals[2], @texCoords[23], Color, index, mesh[5]);

    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
// TQRSphereModel
//--------------------------------------------------------------------------------------------------
constructor TQRSphereModel.Create;
begin
    inherited Create;

    m_Slices := 0;
    m_Stacks := 0;
    m_Radius := 0.0;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRSphereModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRSphereModel.GetMesh(out mesh: TQRMesh;
                               pAABBTree: TQRAABBTree;
                             hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    i,
    j,
    meshIndex,
    fanLength,
    index,
    stride:  NativeUInt;
    majorStep,
    minorStep,
    a,
    b,
    r0,
    r1,
    z0,
    z1,
    c,
    x,
    y,
    si,
    sj,
    sslices,
    sstacks: Single;
begin
    // no slices?
    if (m_Slices = 0) then
        Exit(False);

    // no stacks?
    if (m_Stacks = 0) then
        Exit(False);

    // no radius?
    if (m_Radius = 0.0) then
        Exit(False);

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

    // initialize basic values
    majorStep := (Pi         / m_Slices);
    minorStep := ((2.0 * Pi) / m_Stacks);

    // iterate through vertex m_Slices
    for i := 0 to m_Slices do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        // calculate values for next slice
        a  := i        * majorStep;
        b  := a        + majorStep;
        r0 := m_Radius * Sin(a);
        r1 := m_Radius * Sin(b);
        z0 := m_Radius * Cos(a);
        z1 := m_Radius * Cos(b);

        // calculate current index and slice fan length
        meshIndex := Length(mesh);
        fanLength := (m_Stacks + 1) * stride * 2;

        // adde new mesh in output array
        SetLength(mesh, Length(mesh) + 1);

        // populate mesh
        mesh[meshIndex].m_Name      := 'qr_sphere';
        mesh[meshIndex].m_Stride    := stride;
        mesh[meshIndex].m_Type      := EQR_VT_TriangleStrip;
        mesh[meshIndex].m_Format    := VertexFormat;
        mesh[meshIndex].m_CoordType := EQR_VC_XYZ;
        SetLength(mesh[meshIndex].m_Buffer, fanLength);

        index := 0;

        // iterate through vertex m_Stacks
        for j := 0 to m_Stacks do
        begin
            // is canceled?
            if (Assigned(hIsCanceled) and hIsCanceled) then
                Exit(False);

            c := j * minorStep;
            x := Cos(c);
            y := Sin(c);

            // set vertex data
            mesh[meshIndex].m_Buffer[index]     := x * r0;
            mesh[meshIndex].m_Buffer[index + 1] := y * r0;
            mesh[meshIndex].m_Buffer[index + 2] := z0;

            Inc(index, 3);

            // do generate normals?
            if (EQR_VF_Normals in VertexFormat) then
            begin
                // set normals
                mesh[meshIndex].m_Buffer[index]     := (x * r0) / m_Radius;
                mesh[meshIndex].m_Buffer[index + 1] := (y * r0) / m_Radius;
                mesh[meshIndex].m_Buffer[index + 2] := z0       / m_Radius;

                Inc(index, 3);
            end;

            // do generate texture coordinates?
            if (EQR_VF_TexCoords in VertexFormat) then
            begin
                // convert cardinal values to single to avoid rounding error while dividing values
                si      := i;
                sj      := j;
                sStacks := m_Stacks;
                sSlices := m_Slices;

                // add texture coordinates data to buffer
                mesh[meshIndex].m_Buffer[index]     := (sj / sStacks);
                mesh[meshIndex].m_Buffer[index + 1] := (si / sSlices);

                Inc(index, 2);
            end;

            // do generate colors?
            if (EQR_VF_Colors in VertexFormat) then
            begin
                // set color data
                mesh[meshIndex].m_Buffer[index]     := Color.GetRedF;
                mesh[meshIndex].m_Buffer[index + 1] := Color.GetGreenF;
                mesh[meshIndex].m_Buffer[index + 2] := Color.GetBlueF;
                mesh[meshIndex].m_Buffer[index + 3] := Color.GetAlphaF;

                Inc(index, 4);
            end;

            mesh[meshIndex].m_Buffer[index]     := x * r1;
            mesh[meshIndex].m_Buffer[index + 1] := y * r1;
            mesh[meshIndex].m_Buffer[index + 2] := z1;

            Inc(index, 3);

            // do generate normals?
            if (EQR_VF_Normals in VertexFormat) then
            begin
                // set normals
                mesh[meshIndex].m_Buffer[index]     := (x * r1) / m_Radius;
                mesh[meshIndex].m_Buffer[index + 1] := (y * r1) / m_Radius;
                mesh[meshIndex].m_Buffer[index + 2] :=  z1      / m_Radius;

                Inc(index, 3);
            end;

            // do generate texture coordinates?
            if (EQR_VF_TexCoords in VertexFormat) then
            begin
                // convert cardinal values to single to avoid rounding error while dividing values
                si      := i;
                sj      := j;
                sStacks := m_Stacks;
                sSlices := m_Slices;

                // add texture coordinates data to buffer
                mesh[meshIndex].m_Buffer[index]     := ( sj        / sStacks);
                mesh[meshIndex].m_Buffer[index + 1] := ((si + 1.0) / sSlices);

                Inc(index, 2);
            end;

            // do generate colors?
            if (EQR_VF_Colors in VertexFormat) then
            begin
                // set color data
                mesh[meshIndex].m_Buffer[index]     := Color.GetRedF;
                mesh[meshIndex].m_Buffer[index + 1] := Color.GetGreenF;
                mesh[meshIndex].m_Buffer[index + 2] := Color.GetBlueF;
                mesh[meshIndex].m_Buffer[index + 3] := Color.GetAlphaF;

                Inc(index, 4);
            end;
        end;
    end;

    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
// TQRConeModel
//--------------------------------------------------------------------------------------------------
constructor TQRConeModel.Create;
begin
    inherited Create;

    m_FaceCount     := 0;
    m_Height        := 0.0;
    m_TopRadiusX    := 0.0;
    m_TopRadiusY    := 0.0;
    m_BottomRadiusX := 0.0;
    m_BottomRadiusY := 0.0;
    m_Closing       := EQR_CC_None;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRConeModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRConeModel.GetMesh(out mesh: TQRMesh;
                             pAABBTree: TQRAABBTree;
                           hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    stride,
    i,
    index,
    topIndex,
    bottomIndex,
    meshIndex,
    meshCount:        NativeUInt;
    angle,
    texXOffset,
    texXPos,
    texYTopOffset,
    texYBottomOffset: Single;
    base,
    apex,
    baseCenter,
    apexCenter,
    normal,
    apexNormal,
    baseNormal,
    nextApex:         TQRVector3D;
    texStartCoord,
    texEndCoord:      TQRVector2D;
    topClosing,
    bottomClosing:    Boolean;
begin
    // no face count?
    if (m_FaceCount = 0) then
        Exit(False);

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

    // check if top closing surface should be applied
    topClosing := ((m_Closing = EQR_CC_Top) or (m_Closing = EQR_CC_Both) and
                   (m_TopRadiusX <> 0.0)                                 and
                   (m_TopRadiusY <> 0.0));

    // check if bottom closing surface should be applied
    bottomClosing := ((m_Closing = EQR_CC_Bottom) or (m_Closing = EQR_CC_Both) and
                      (m_BottomRadiusX <> 0.0)                                 and
                      (m_BottomRadiusY <> 0.0));

    meshCount := 1;

    // calculate apex center vertex, if needed
    if (topClosing) then
    begin
        apexNormal := TQRVector3D.Create(0.0, 1.0,            0.0);
        apexCenter := TQRVector3D.Create(0.0, m_Height / 2.0, 0.0);
        Inc(meshCount);
    end;

    // calculate base center vertex, if needed
    if (bottomClosing) then
    begin
        baseNormal := TQRVector3D.Create(0.0, -1.0,            0.0);
        baseCenter := TQRVector3D.Create(0.0, -m_Height / 2.0, 0.0);
        Inc(meshCount);
    end;

    // reserve memory for meshes
    SetLength(mesh, meshCount);

    // initialize meshes
    for i := 0 to meshCount - 1 do
    begin
        mesh[i].m_Name      := 'qr_cone';
        mesh[i].m_Stride    := stride;
        mesh[i].m_Type      := EQR_VT_TriangleStrip;
        mesh[i].m_Format    := VertexFormat;
        mesh[i].m_CoordType := EQR_VC_XYZ;
    end;

    // calculate angle to apply between each face
    angle := (2 * PI) / m_FaceCount;

    // calculate texture offsets
    texYTopOffset    := 1.0 / 3.0;
    texYBottomOffset := 2.0 / 3.0;
    texXOffset       := 1.0 / m_FaceCount;

    index       := 0;
    topIndex    := 0;
    bottomIndex := 0;

    // calculate cone vertices
    for i := 0 to m_FaceCount do
    begin
        meshIndex := 0;

        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        // calculate apex and base vertices for current subdivision
        apex := TQRVector3D.Create(m_TopRadiusX    * Cos(angle * i),
                                   m_Height / 2.0,
                                   m_TopRadiusY    * Sin(angle * i));
        base := TQRVector3D.Create(m_BottomRadiusX * Cos(angle * i),
                                  -m_Height / 2.0,
                                   m_BottomRadiusY * Sin(angle * i));

        // do include normals?
        if (EQR_VF_Normals in VertexFormat) then
        begin
            // calculate next apex position
            nextApex := TQRVector3D.Create(m_TopRadiusX * Cos(angle * (i + 1)),
                                           m_Height / 2.0,
                                           m_TopRadiusY * Sin(angle * (i + 1)));

            // calculate normal
            if ((((m_TopRadiusX    < 0.0) or (m_TopRadiusY    < 0.0)) and (i <  m_FaceCount / 2)) or
                (((m_BottomRadiusX < 0.0) or (m_BottomRadiusY < 0.0)) and (i >= m_FaceCount / 2)))
            then
                normal := TQRShapeHelper.NormalFromPoints(@apex, @base, @nextApex)
            else
                normal := TQRShapeHelper.NormalFromPoints(@apex, @nextApex, @base);
        end;

        // calculate texture x offset for this subdivision
        texXPos := texXOffset * (m_FaceCount - i);

        // do include texture coordinates?
        if (EQR_VF_TexCoords in VertexFormat) then
        begin
            texStartCoord := TQRVector2D.Create(texXPos, texYTopOffset);
            texEndCoord   := TQRVector2D.Create(texXPos, texYBottomOffset);
        end;

        // build cone vertices
        TQRShapeHelper.AddVertex(@base, @normal, @texEndCoord,   Color, index, mesh[meshIndex]);
        TQRShapeHelper.AddVertex(@apex, @normal, @texStartCoord, Color, index, mesh[meshIndex]);
        Inc(meshIndex);

        // do create top closing surface?
        if (topClosing) then
        begin
            // is canceled?
            if (Assigned(hIsCanceled) and hIsCanceled) then
                Exit(False);

            // do include texture coordinates?
            if (EQR_VF_TexCoords in VertexFormat) then
            begin
                texStartCoord := TQRVector2D.Create(texXPos, texYTopOffset);
                texEndCoord   := TQRVector2D.Create(texXPos, 0.0);
            end;

            // build top closing surface vertices
            TQRShapeHelper.AddVertex(@apex,
                                     @apexNormal,
                                     @texStartCoord,
                                     Color,
                                     topIndex,
                                     mesh[meshIndex]);
            TQRShapeHelper.AddVertex(@apexCenter,
                                     @apexNormal,
                                     @texEndCoord,
                                     Color,
                                     topIndex,
                                     mesh[meshIndex]);

            Inc(meshIndex);
        end;

        // do create bottom closing surface?
        if (bottomClosing) then
        begin
            // is canceled?
            if (Assigned(hIsCanceled) and hIsCanceled) then
                Exit(False);

            // do include texture coordinates?
            if (EQR_VF_TexCoords in VertexFormat) then
            begin
                texStartCoord := TQRVector2D.Create(texXPos, 1.0);
                texEndCoord   := TQRVector2D.Create(texXPos, texYBottomOffset);
            end;

            // build bottom closing surface vertices
            TQRShapeHelper.AddVertex(@baseCenter,
                                     @baseNormal,
                                     @texStartCoord,
                                     Color,
                                     bottomIndex,
                                     mesh[meshIndex]);
            TQRShapeHelper.AddVertex(@base,
                                     @baseNormal,
                                     @texEndCoord,
                                     Color,
                                     bottomIndex,
                                     mesh[meshIndex]);
        end;
    end;

    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
// TQRTorusModel
//--------------------------------------------------------------------------------------------------
constructor TQRTorusModel.Create;
begin
    inherited Create;

    m_Slices         := 0;
    m_FacesPerSlices := 0;
    m_OuterRadiusX   := 0.0;
    m_OuterRadiusY   := 0.0;
    m_InnerRadiusX   := 0.0;
    m_InnerRadiusY   := 0.0;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRTorusModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRTorusModel.GetMesh(out mesh: TQRMesh;
                              pAABBTree: TQRAABBTree;
                            hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    stride, i, u, v, index, meshCount:                                   NativeUInt;
    totalAngle, theta, nextTheta, phi, uStep, vStep, uTexStep, vTexStep: Single;
    vertex, normal:                                                      TQRVector3D;
    texCoord:                                                            TQRVector2D;
begin
    // no slice count?
    if (m_Slices = 0) then
        Exit(False);

    // no faces per slice count?
    if (m_FacesPerSlices = 0) then
        Exit(False);

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

    meshCount := m_Slices;

    // reserve memory for meshes
    SetLength(mesh, meshCount);

    // initialize meshes
    for i := 0 to meshCount - 1 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        mesh[i].m_Name      := 'qr_torus';
        mesh[i].m_Stride    := stride;
        mesh[i].m_Type      := EQR_VT_TriangleStrip;
        mesh[i].m_Format    := VertexFormat;
        mesh[i].m_CoordType := EQR_VC_XYZ;
    end;

    totalAngle := 2 * PI;
    uStep      := totalAngle / m_Slices;
    vStep      := totalAngle / m_FacesPerSlices;
    uTexStep   := 1.0        / m_Slices;
    vTexStep   := 1.0        / m_FacesPerSlices;

    // iterate through slices to create
    for u := 0 to m_Slices - 1 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        theta     := ( u      * uStep);
        nextTheta := ((u + 1) * uStep);
        index     := 0;

        // iterate through faces to create
        for v := 0 to m_FacesPerSlices do
        begin
            // is canceled?
            if (Assigned(hIsCanceled) and hIsCanceled) then
                Exit(False);

            phi := (v * vStep);

            // calculate vertex
            vertex.X := Cos(theta) * (m_OuterRadiusX + m_InnerRadiusX * Cos(phi));
            vertex.Y := Sin(theta) * (m_OuterRadiusY + m_InnerRadiusX * Cos(phi));
            vertex.Z :=                                m_InnerRadiusY * Sin(phi);

            // do include normals?
            if (EQR_VF_Normals in VertexFormat) then
            begin
                // calculate normal
                normal.X := Cos(theta) * Cos(phi);
                normal.Y := Sin(theta) * Cos(phi);
                normal.Z :=              Sin(phi);
            end;

            // do include texture coordinates?
            if (EQR_VF_TexCoords in VertexFormat) then
            begin
                texCoord.X := v * vTexStep;
                texCoord.Y := u * uTexStep;
            end;

            // build slice start vertices
            TQRShapeHelper.AddVertex(@vertex,
                                     @normal,
                                     @texCoord,
                                     Color,
                                     index,
                                     mesh[u]);

            // calculate next vertex
            vertex.X := Cos(nextTheta) * (m_OuterRadiusX + m_InnerRadiusX * Cos(phi));
            vertex.Y := Sin(nextTheta) * (m_OuterRadiusY + m_InnerRadiusX * Cos(phi));
            vertex.Z :=                                    m_InnerRadiusY * Sin(phi);

            // do include normals?
            if (EQR_VF_Normals in VertexFormat) then
            begin
                // calculate next normal
                normal.X := Cos(nextTheta) * Cos(phi);
                normal.Y := Sin(nextTheta) * Cos(phi);
                normal.Z :=                  Sin(phi);
            end;

            // do include texture coordinates?
            if (EQR_VF_TexCoords in VertexFormat) then
            begin
                texCoord.X :=  v *      vTexStep;
                texCoord.Y := (u + 1) * uTexStep;
            end;

            // build slice end vertices
            TQRShapeHelper.AddVertex(@vertex,
                                     @normal,
                                     @texCoord,
                                     Color,
                                     index,
                                     mesh[u]);
        end;
    end;

    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
// TQRParabolaModel
//--------------------------------------------------------------------------------------------------
constructor TQRParabolaModel.Create;
begin
    inherited Create;

    m_Slices         := 0;
    m_FacesPerSlices := 0;
    m_Radius         := 0.0;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRParabolaModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRParabolaModel.GetMesh(out mesh: TQRMesh;
                                 pAABBTree: TQRAABBTree;
                               hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    stride,
    i,
    u,
    v,
    extIndex,
    intIndex,
    meshCount:    NativeUInt;
    totalAngle,
    theta,
    nextTheta,
    uStep,
    vStep,
    texStep,
    radiusStep,
    r0,
    r1: Single;
    vertex,
    nextVertex,
    nextFaceVertex,
    extNormal,
    intNormal:    TQRVector3D;
    texCoord,
    nextTexCoord: TQRVector2D;
begin
    // no slice count?
    if (m_Slices = 0) then
        Exit(False);

    // no faces per slice count?
    if (m_FacesPerSlices = 0) then
        Exit(False);

    // negative radius or no radius?
    if (m_Radius <= 0.0) then
        Exit(False);

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

    meshCount := m_Slices * 2;

    // reserve memory for meshes
    SetLength(mesh, meshCount);

    // initialize meshes
    for i := 0 to meshCount - 1 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        mesh[i].m_Name      := 'qr_parabola';
        mesh[i].m_Stride    := stride;
        mesh[i].m_Type      := EQR_VT_TriangleStrip;
        mesh[i].m_Format    := VertexFormat;
        mesh[i].m_CoordType := EQR_VC_XYZ;
    end;

    totalAngle := 2 * PI;
    uStep      := m_Height   / m_Slices;
    vStep      := totalAngle / m_FacesPerSlices;
    radiusStep := m_Radius   / m_Slices;
    texStep    := 1.0        / m_Slices;

    // iterate through slices to create
    for u := 0 to m_Slices - 1 do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit(False);

        extIndex := 0;
        intIndex := 0;

        // iterate through faces to create
        for v := 0 to m_FacesPerSlices do
        begin
            // is canceled?
            if (Assigned(hIsCanceled) and hIsCanceled) then
                Exit(False);

            theta := (v * vStep);

            // calculate parabolic position in relation to radius
            r0 := Sqrt(radiusStep *  u);
            r1 := Sqrt(radiusStep * (u + 1));

            // calculate start vertex
            vertex.X := r0 * Cos(theta);
            vertex.Y := r0 * Sin(theta);
            vertex.Z := u  * uStep;

            // calculate end vertex
            nextVertex.X := r1      * Cos(theta);
            nextVertex.Y := r1      * Sin(theta);
            nextVertex.Z := (u + 1) * uStep;

            // do include normals?
            if (EQR_VF_Normals in VertexFormat) then
            begin
                nextTheta := ((v + 1) * vStep);

                // calculate end vertex
                nextFaceVertex.X :=  r1     * Cos(nextTheta);
                nextFaceVertex.Y :=  r1     * Sin(nextTheta);
                nextFaceVertex.Z := (u + 1) * uStep;

                // calculate normals
                extNormal := TQRShapeHelper.NormalFromPoints(@nextVertex, @vertex,         @nextFaceVertex);
                intNormal := TQRShapeHelper.NormalFromPoints(@nextVertex, @nextFaceVertex, @vertex);
            end;

            // do include texture coordinates?
            if (EQR_VF_TexCoords in VertexFormat) then
            begin
                texCoord.X     := 0.5 + (((u       * texStep) / 2.0) * Cos(theta));
                texCoord.Y     := 0.5 + (((u       * texStep) / 2.0) * Sin(theta));
                nextTexCoord.X := 0.5 + ((((u + 1) * texStep) / 2.0) * Cos(theta));
                nextTexCoord.Y := 0.5 + ((((u + 1) * texStep) / 2.0) * Sin(theta));
            end;

            // build external slice start vertex
            TQRShapeHelper.AddVertex(@nextVertex,
                                     @extNormal,
                                     @texCoord,
                                     Color,
                                     extIndex,
                                     mesh[u * 2]);

            // build external slice end vertex
            TQRShapeHelper.AddVertex(@vertex,
                                     @extNormal,
                                     @nextTexCoord,
                                     Color,
                                     extIndex,
                                     mesh[u * 2]);

            // build internal slice start vertex
            TQRShapeHelper.AddVertex(@vertex,
                                     @intNormal,
                                     @texCoord,
                                     Color,
                                     intIndex,
                                     mesh[(u * 2) + 1]);

            // build internal slice end vertex
            TQRShapeHelper.AddVertex(@nextVertex,
                                     @intNormal,
                                     @nextTexCoord,
                                     Color,
                                     intIndex,
                                     mesh[(u * 2) + 1]);
        end;
    end;

    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------

end.
