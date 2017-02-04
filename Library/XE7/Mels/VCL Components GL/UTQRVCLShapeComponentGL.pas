// *************************************************************************************************
// * ==> UTQRVCLShapeComponentGL ------------------------------------------------------------------*
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
 @abstract(@name provides a set of shapes that can be drawn using OpenGL.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRVCLShapeComponentGL;

interface
    // do not include Winapi.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.OpenGLext *)

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
     Winapi.OpenGL,
     Winapi.OpenGLext;

type
    {$REGION 'Documentation'}
    {**
     Shapes component
    }
    {$ENDREGION}
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

            {$REGION 'Documentation'}
            {**
             Sets texture
             @param(pPicture Texture picture)
            }
            {$ENDREGION}
            procedure SetTexture(pPicture: TPicture); virtual;

            {$REGION 'Documentation'}
            {**
             Sets model options
             @param(options Options)
            }
            {$ENDREGION}
            procedure SetModelOptions(options: TQRModelOptions); virtual;

            {$REGION 'Documentation'}
            {**
             Sets model vertex shader file name
             @param(fileName File name)
            }
            {$ENDREGION}
            procedure SetVertexName(fileName: TFileName); virtual;

            {$REGION 'Documentation'}
            {**
             Sets model fragment shader file name
             @param(fileName File name)
            }
            {$ENDREGION}
            procedure SetFragmentName(fileName: TFileName); virtual;

            {$REGION 'Documentation'}
            {**
             Declares properties that will deal with DFM files
             @param(pFiler DFM file manager)
            }
            {$ENDREGION}
            procedure DefineProperties(pFiler: TFiler); override;

            {$REGION 'Documentation'}
            {**
             Reads vertex shader content from DFM file
             @param(pStream Stream containing DFM data)
            }
            {$ENDREGION}
            procedure ReadVertexShader(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Writes vertex shader content to DFM file
             @param(pStream DFM stream in which vertex shader should be written)
            }
            {$ENDREGION}
            procedure WriteVertexShader(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Reads fragment shader content from DFM file
             @param(pStream Stream containing DFM data)
            }
            {$ENDREGION}
            procedure ReadFragmentShader(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Writes fragment shader content to DFM file
             @param(pStream DFM stream in which fragment shader should be written)
            }
            {$ENDREGION}
            procedure WriteFragmentShader(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Creates the component Windows handle
             @param(params Windows parameters used to create handle)
            }
            {$ENDREGION}
            procedure CreateWindowHandle(const params: TCreateParams); override;

            {$REGION 'Documentation'}
            {**
             Deletes the component Windows handle
            }
            {$ENDREGION}
            procedure DestroyWindowHandle; override;

            {$REGION 'Documentation'}
            {**
             Loads the model
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadModel: Boolean; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Called after model was completely loaded
             @param(pGroup Group that finished to load the model)
            }
            {$ENDREGION}
            procedure OnAfterLoadModelEvent(const pGroup: TQRModelGroup); virtual;

            {$REGION 'Documentation'}
            {**
             Called when texture changed
             @param(pSender Event sender)
            }
            {$ENDREGION}
            procedure OnTextureChanged(pSender: TObject); virtual;

            {$REGION 'Documentation'}
            {**
             Called when mesh texture should be loaded
             @param(pModel Model for which texture should be loaded)
             @param(pBitmap Whenever possible, the bitmap containing the texture, @nil if not
                            available)
             @param(pTexture Texture info)
             @param(loadNext @bold([out]) If @true, event will be called again with a new item to
                                          load next texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                       const pModel: TQRModel;
                                            pBitmap: Vcl.Graphics.TBitmap;
                                           pTexture: TQRTexture;
                                       out loadNext: Boolean): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Called when the scene content should be drawn
             @param(hDC Internal control device context that OpenGL should use to draw the scene)
            }
            {$ENDREGION}
            procedure OnDrawSceneContent(hDC: THandle); override;

            {$REGION 'Documentation'}
            {**
             Called when framed model item should be drawn
             @param(pGroup Group at which model belongs)
             @param(pModel Model to draw)
             @param(textures Textures belonging to model, in the order where they should be combined)
             @param(matrix Model matrix)
            }
            {$ENDREGION}
            procedure OnCustomDrawModelItem(const pGroup: TQRModelGroup;
                                                  pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4); virtual;

            {$REGION 'Documentation'}
            {**
             Called when framed model item should be drawn
             @param(pGroup Group at which model belongs)
             @param(pModel Model to draw)
             @param(textures Textures belonging to model, in the order where they should be combined)
             @param(matrix Model matrix)
             @param(pMesh Mesh to draw, can be @nil)
             @param(pAABBTree Aligned-axis bounding box tree matching with mesh, can be @nil)
            }
            {$ENDREGION}
            procedure OnDrawModelItem(const pGroup: TQRModelGroup;
                                      const pModel: TQRModel;
                                    const textures: TQRTextures;
                                      const matrix: TQRMatrix4x4;
                                       const pMesh: PQRMesh;
                                   const pAABBTree: TQRAABBTree); virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner Component owner)
            }
            {$ENDREGION}
            constructor Create(pOwner: TComponent); override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Copies the property attributes from another property
             @param(pSource Source property to copy from)
            }
            {$ENDREGION}
            procedure Assign(pSource: TPersistent); override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the shape
            }
            {$ENDREGION}
            property Shape: TQRShapeGroup read m_pShape;

        // Properties
        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the model properties
            }
            {$ENDREGION}
            property Model: TQRVCLModelComponentPropertyGL read m_pModel write m_pModel;

            {$REGION 'Documentation'}
            {**
             Gets or sets the model options, default are EQR_MO_Dynamic_Frames and EQR_MO_No_Collision
            }
            {$ENDREGION}
            property ModelOptions: TQRModelOptions read m_ModelOptions write SetModelOptions default [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];

            {$REGION 'Documentation'}
            {**
             Gets or sets the vertex shader file name to load
            }
            {$ENDREGION}
            property VertexName: TFileName read m_VertexName write SetVertexName;

            {$REGION 'Documentation'}
            {**
             Gets or sets the fragment shader file name to load
            }
            {$ENDREGION}
            property FragmentName: TFileName read m_FragmentName write SetFragmentName;

            {$REGION 'Documentation'}
            {**
             Gets or sets the texture to use
            }
            {$ENDREGION}
            property Texture: TPicture read m_pTexture write SetTexture;
    end;

    {$REGION 'Documentation'}
    {**
     Surface component
    }
    {$ENDREGION}
    TQRVCLSurfaceGL = class(TQRVCLShapeGL)
        private
            m_SurfaceWidth:  Single;
            m_SurfaceHeight: Single;

        protected
            {$REGION 'Documentation'}
            {**
             Sets surface width
             @param(width Surface width)
            }
            {$ENDREGION}
            procedure SetSurfaceWidth(width: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets surface height
             @param(height Surface height)
            }
            {$ENDREGION}
            procedure SetSurfaceHeight(height: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Loads the model
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadModel: Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner Component owner)
            }
            {$ENDREGION}
            constructor Create(pOwner: TComponent); override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Copies the property attributes from another property
             @param(pSource Source property to copy from)
            }
            {$ENDREGION}
            procedure Assign(pSource: TPersistent); override;

        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the surface length on the x axis
             @br @br
             @image(Resources/Images/Documentation/Surface width.svg)
            }
            {$ENDREGION}
            property SurfaceWidth: Single read m_SurfaceWidth write SetSurfaceWidth;

            {$REGION 'Documentation'}
            {**
             Gets or sets the surface length on the y axis
             @br @br
             @image(Resources/Images/Documentation/Surface height.svg)
            }
            {$ENDREGION}
            property SurfaceHeight: Single read m_SurfaceHeight write SetSurfaceHeight;
    end;

    {$REGION 'Documentation'}
    {**
     Box component
    }
    {$ENDREGION}
    TQRVCLBoxGL = class(TQRVCLShapeGL)
        private
            m_BoxWidth:            Single;
            m_BoxHeight:           Single;
            m_BoxDepth:            Single;
            m_RepeatTexOnEachFace: Boolean;

        protected
            {$REGION 'Documentation'}
            {**
             Sets box width
             @param(width Box width)
            }
            {$ENDREGION}
            procedure SetBoxWidth(width: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets box height
             @param(height Box height)
            }
            {$ENDREGION}
            procedure SetBoxHeight(height: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets box depth
             @param(depth Box depth)
            }
            {$ENDREGION}
            procedure SetBoxDepth(depth: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Loads the model
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadModel: Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner Component owner)
            }
            {$ENDREGION}
            constructor Create(pOwner: TComponent); override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Copies the property attributes from another property
             @param(pSource Source property to copy from)
            }
            {$ENDREGION}
            procedure Assign(pSource: TPersistent); override;

        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the box length on the x axis
             @br @br
             @image(Resources/Images/Documentation/Cube width.svg)
            }
            {$ENDREGION}
            property BoxWidth: Single read m_BoxWidth write SetBoxWidth;

            {$REGION 'Documentation'}
            {**
             Gets or sets the box length on the y axis
             @br @br
             @image(Resources/Images/Documentation/Cube height.svg)
            }
            {$ENDREGION}
            property BoxHeight: Single read m_BoxHeight write SetBoxHeight;

            {$REGION 'Documentation'}
            {**
             Gets or sets the box length on the z axis
             @br @br
             @image(Resources/Images/Documentation/Cube depth.svg)
            }
            {$ENDREGION}
            property BoxDepth: Single read m_BoxDepth write SetBoxDepth;
    end;

    {$REGION 'Documentation'}
    {**
     Sphere component
    }
    {$ENDREGION}
    TQRVCLSphereGL = class(TQRVCLShapeGL)
        private
            m_Slices: NativeUInt;
            m_Stacks: NativeUInt;
            m_Radius: Single;

        protected
            {$REGION 'Documentation'}
            {**
             Sets slices
             @param(slices Sphere slices)
            }
            {$ENDREGION}
            procedure SetSlices(slices: NativeUInt); virtual;

            {$REGION 'Documentation'}
            {**
             Sets stacks
             @param(stacks Sphere stacks)
            }
            {$ENDREGION}
            procedure SetStacks(stacks: NativeUInt); virtual;

            {$REGION 'Documentation'}
            {**
             Sets sphere radius
             @param(radius Sphere radius)
            }
            {$ENDREGION}
            procedure SetRadius(radius: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Loads the model
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadModel: Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner Component owner)
            }
            {$ENDREGION}
            constructor Create(pOwner: TComponent); override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Copies the property attributes from another property
             @param(pSource Source property to copy from)
            }
            {$ENDREGION}
            procedure Assign(pSource: TPersistent); override;

        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the slices count that will be used to divide the sphere while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Sphere Slices.svg)
            }
            {$ENDREGION}
            property Slices: NativeUInt read m_Slices write SetSlices default 20;

            {$REGION 'Documentation'}
            {**
             Gets or sets the stacks count that will be used to divide the sphere while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Sphere Stacks.svg)
            }
            {$ENDREGION}
            property Stacks: NativeUInt read m_Stacks write SetStacks default 20;

            {$REGION 'Documentation'}
            {**
             Gets or sets the sphere radius
             @br @br
             @image(Resources/Images/Documentation/Sphere Radius.svg)
            }
            {$ENDREGION}
            property Radius: Single read m_Radius write SetRadius;
    end;

    {$REGION 'Documentation'}
    {**
     Cone component
    }
    {$ENDREGION}
    TQRVCLConeGL = class(TQRVCLShapeGL)
        private
            m_Faces:         NativeUInt;
            m_ConeHeight:    Single;
            m_TopRadiusX:    Single;
            m_TopRadiusY:    Single;
            m_BottomRadiusX: Single;
            m_BottomRadiusY: Single;
            m_Closing:       EQR_Cone_Closing;

        protected
            {$REGION 'Documentation'}
            {**
             Sets cone faces count
             @param(faces Cone faces)
            }
            {$ENDREGION}
            procedure SetFaces(faces: NativeUInt); virtual;

            {$REGION 'Documentation'}
            {**
             Sets cone height
             @param(height Cone height)
            }
            {$ENDREGION}
            procedure SetConeHeight(height: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets cone top radius for the x axis
             @param(topRadiusX Top radius for the x axis)
            }
            {$ENDREGION}
            procedure SetTopRadiusX(topRadiusX: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets cone top radius for the y axis
             @param(topRadiusY Top radius for the y axis)
            }
            {$ENDREGION}
            procedure SetTopRadiusY(topRadiusY: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets cone bottom radius for the x axis
             @param(bottomRadiusX Bottom radius for the x axis)
            }
            {$ENDREGION}
            procedure SetBottomRadiusX(bottomRadiusX: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets cone bottom radius for the y axis
             @param(bottomRadiusX Bottom radius for the y axis)
            }
            {$ENDREGION}
            procedure SetBottomRadiusY(bottomRadiusY: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets cone closing method
             @param(closing Cone closing method)
            }
            {$ENDREGION}
            procedure SetClosing(closing: EQR_Cone_Closing); virtual;

            {$REGION 'Documentation'}
            {**
             Loads the model
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadModel: Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner Component owner)
            }
            {$ENDREGION}
            constructor Create(pOwner: TComponent); override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Copies the property attributes from another property
             @param(pSource Source property to copy from)
            }
            {$ENDREGION}
            procedure Assign(pSource: TPersistent); override;

        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the number of faces to generate while cone vertex buffer will be created
             @br @br
             @image(Resources/Images/Documentation/Cone Face Count.svg)
            }
            {$ENDREGION}
            property Faces: NativeUInt read m_Faces write SetFaces default 20;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone height
             @br @br
             @image(Resources/Images/Documentation/Cone Height.svg)
            }
            {$ENDREGION}
            property ConeHeight: Single read m_ConeHeight write SetConeHeight;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone top radius on the x axis
             @br @br
             @image(Resources/Images/Documentation/Cone Radius Top X.svg)
            }
            {$ENDREGION}
            property TopRadiusX: Single read m_TopRadiusX write SetTopRadiusX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone top radius on the y axis
             @br @br
             @image(Resources/Images/Documentation/Cone Radius Top Y.svg)
            }
            {$ENDREGION}
            property TopRadiusY: Single read m_TopRadiusY write SetTopRadiusY;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone bottom radius on the x axis
             @br @br
             @image(Resources/Images/Documentation/Cone Radius Bottom X.svg)
            }
            {$ENDREGION}
            property BottomRadiusX: Single read m_BottomRadiusX write SetBottomRadiusX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone bottom radius on the y axis
             @br @br
             @image(Resources/Images/Documentation/Cone Radius Bottom Y.svg)
            }
            {$ENDREGION}
            property BottomRadiusY: Single read m_BottomRadiusY write SetBottomRadiusY;

            {$REGION 'Documentation'}
            {**
             Gets or sets the cone closing
            }
            {$ENDREGION}
            property Closing: EQR_Cone_Closing read m_Closing write SetClosing default EQR_CC_Both;
    end;

    {$REGION 'Documentation'}
    {**
     Torus component
    }
    {$ENDREGION}
    TQRVCLTorusGL = class(TQRVCLShapeGL)
        private
            m_Slices:         NativeUInt;
            m_FacesPerSlices: NativeUInt;
            m_OuterRadiusX:   Single;
            m_OuterRadiusY:   Single;
            m_InnerRadiusX:   Single;
            m_InnerRadiusY:   Single;

        protected
            {$REGION 'Documentation'}
            {**
             Sets torus slices count
             @param(slices Torus slices)
            }
            {$ENDREGION}
            procedure SetSlices(slices: NativeUInt); virtual;

            {$REGION 'Documentation'}
            {**
             Sets torus faces per slices count
             @param(facesPerSlices Torus faces per slices)
            }
            {$ENDREGION}
            procedure SetFacesPerSlices(facesPerSlices: NativeUInt); virtual;

            {$REGION 'Documentation'}
            {**
             Sets torus inner radius for the x axis
             @param(innerRadiusX Inner radius for the x axis)
            }
            {$ENDREGION}
            procedure SetInnerRadiusX(innerRadiusX: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets torus inner radius for the y axis
             @param(innerRadiusY Inner radius for the y axis)
            }
            {$ENDREGION}
            procedure SetInnerRadiusY(innerRadiusY: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets torus outer radius for the x axis
             @param(outerRadiusX Outer radius for the x axis)
            }
            {$ENDREGION}
            procedure SetOuterRadiusX(outerRadiusX: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets torus outer radius for the y axis
             @param(outerRadiusX Outer radius for the y axis)
            }
            {$ENDREGION}
            procedure SetOuterRadiusY(outerRadiusY: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Loads the model
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadModel: Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner Component owner)
            }
            {$ENDREGION}
            constructor Create(pOwner: TComponent); override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Copies the property attributes from another property
             @param(pSource Source property to copy from)
            }
            {$ENDREGION}
            procedure Assign(pSource: TPersistent); override;

        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the slices count that will be used to divide the torus while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Torus Slices.svg)
            }
            {$ENDREGION}
            property Slices: NativeUInt read m_Slices write SetSlices default 20;

            {$REGION 'Documentation'}
            {**
             Gets or sets the number of faces per slices to apply to the torus while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Torus Faces Per Slices.svg)
            }
            {$ENDREGION}
            property FacesPerSlices: NativeUInt read m_FacesPerSlices write SetFacesPerSlices default 20;

            {$REGION 'Documentation'}
            {**
             Gets or sets the torus inner radius on the x axis
             @br @br
             @image(Resources/Images/Documentation/Torus inner Radius X.svg)
            }
            {$ENDREGION}
            property InnerRadiusX: Single read m_InnerRadiusX write SetInnerRadiusX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the torus inner radius on the y axis
             @br @br
             @image(Resources/Images/Documentation/Torus Inner Radius Y.svg)
            }
            {$ENDREGION}
            property InnerRadiusY: Single read m_InnerRadiusY write SetInnerRadiusY;

            {$REGION 'Documentation'}
            {**
             Gets or sets the torus outer radius on the x axis
             @br @br
             @image(Resources/Images/Documentation/Torus Outer Radius X.svg)
            }
            {$ENDREGION}
            property OuterRadiusX: Single read m_OuterRadiusX write SetOuterRadiusX;

            {$REGION 'Documentation'}
            {**
             Gets or sets the torus outer radius on the y axis
             @br @br
             @image(Resources/Images/Documentation/Torus Outer Radius Y.svg)
            }
            {$ENDREGION}
            property OuterRadiusY: Single read m_OuterRadiusY write SetOuterRadiusY;
    end;

    {$REGION 'Documentation'}
    {**
     Parabola component
    }
    {$ENDREGION}
    TQRVCLParabolaGL = class(TQRVCLShapeGL)
        private
            m_Slices:         NativeUInt;
            m_FacesPerSlices: NativeUInt;
            m_ParabolaHeight: Single;
            m_Radius:         Single;

        protected
            {$REGION 'Documentation'}
            {**
             Sets parabola slices count
             @param(slices Parabola slices)
            }
            {$ENDREGION}
            procedure SetSlices(slices: NativeUInt); virtual;

            {$REGION 'Documentation'}
            {**
             Sets parabola faces per slices count
             @param(facesPerSlices Parabola faces per slices)
            }
            {$ENDREGION}
            procedure SetFacesPerSlices(facesPerSlices: NativeUInt); virtual;

            {$REGION 'Documentation'}
            {**
             Sets parabola height
             @param(height Parabola height)
            }
            {$ENDREGION}
            procedure SetParabolaHeight(height: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Sets parabola radius
             @param(radius Parabola radius)
            }
            {$ENDREGION}
            procedure SetRadius(radius: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Loads the model
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadModel: Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner Component owner)
            }
            {$ENDREGION}
            constructor Create(pOwner: TComponent); override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Copies the property attributes from another property
             @param(pSource Source property to copy from)
            }
            {$ENDREGION}
            procedure Assign(pSource: TPersistent); override;

        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the slices count that will be used to divide the parabola while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Parabola Slices.svg)
            }
            {$ENDREGION}
            property Slices: NativeUInt read m_Slices write SetSlices;

            {$REGION 'Documentation'}
            {**
             Gets or sets the number of faces per slices to apply to the parabola while his vertex
             buffer is built
             @br @br
             @image(Resources/Images/Documentation/Parabola Faces Per Slices.svg)
            }
            {$ENDREGION}
            property FacesPerSlices: NativeUInt read m_FacesPerSlices write SetFacesPerSlices;

            {$REGION 'Documentation'}
            {**
             Gets or sets the parabola height
             @br @br
             @image(Resources/Images/Documentation/Parabola Height.svg)
            }
            {$ENDREGION}
            property ParabolaHeight: Single read m_ParabolaHeight write SetParabolaHeight;

            {$REGION 'Documentation'}
            {**
             Gets or sets the parabola radius
             @br @br
             @image(Resources/Images/Documentation/Parabola Radius.svg)
            }
            {$ENDREGION}
            property Radius: Single read m_Radius write SetRadius;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLShapeGL
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.SetTexture(pPicture: TPicture);
begin
    m_pTexture.Assign(pPicture);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.SetModelOptions(options: TQRModelOptions);
begin
    // no changes?
    if (m_ModelOptions = options) then
        Exit;

    m_ModelOptions := options;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.ReadVertexShader(pStream: TStream);
begin
    // previous vertex shader was loaded?
    if (m_pVertexShader.Size > 0) then
        // clear it
        m_pVertexShader.Clear;

    // read vertex shader from DFM stream
    m_pVertexShader.CopyFrom(pStream, pStream.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.WriteVertexShader(pStream: TStream);
begin
    // reset stream position to start
    m_pVertexShader.Position := 0;

    // write vertex shader to DFM stream
    pStream.CopyFrom(m_pVertexShader, m_pVertexShader.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.ReadFragmentShader(pStream: TStream);
begin
    // previous fragment shader was loaded?
    if (m_pFragmentShader.Size > 0) then
        // clear it
        m_pFragmentShader.Clear;

    // read fragment shader from DFM stream
    m_pFragmentShader.CopyFrom(pStream, pStream.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.WriteFragmentShader(pStream: TStream);
begin
    // reset stream position to start
    m_pFragmentShader.Position := 0;

    // write fragment shader to DFM stream
    pStream.CopyFrom(m_pFragmentShader, m_pFragmentShader.Size);
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.DestroyWindowHandle;
begin
    // as model is linked to the current context, clears it if context is shutting down
    if (Assigned(m_pShape)) then
        m_pShape.Clear;

    inherited DestroyWindowHandle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.OnAfterLoadModelEvent(const pGroup: TQRModelGroup);
begin
    SetModelLoaded(True);

    // invalidate model to repaint it
    Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.OnTextureChanged(pSender: TObject);
begin
    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
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
    // no model?
    if (not Assigned(pModel)) then
        Exit(False);

    // no destination texture?
    if (not Assigned(pTexture)) then
        Exit(False);

    // is source texture empty?
    if ((m_pTexture.Width = 0) or (m_pTexture.Height = 0)) then
        Exit(False);

    // check if handle was successfully allocated
    if (not HandleAllocated) then
        Exit(False);

    // get the device context for this control
    hDC := GetDC(WindowHandle);

    // found it?
    if (hDC = 0) then
        Exit(False);

    try
        // enable OpenGL rendering context
        if (not RenderSurface.EnableContext(hDC)) then
            Exit(False);

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
                pTexture.Index := Renderer.CreateTexture(pSrcBitmap.Width,
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

        // notify user that a texture should be loaded for the model
        if ((not(csDesigning in ComponentState)) and Assigned(OnLoadTexture)) then
            OnLoadTexture(Self,
                          hDC,
                          RenderSurface.GLContext,
                          Renderer,
                          Shader);
    finally
        ReleaseDC(WindowHandle, hDC);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.OnCustomDrawModelItem(const pGroup: TQRModelGroup;
                                                    pModel: TQRModel;
                                            const textures: TQRTextures;
                                              const matrix: TQRMatrix4x4);
begin
    if ((csDesigning in ComponentState) or not(Assigned(OnDrawSceneStaticModelItem))) then
        Exit;

    OnDrawSceneStaticModelItem(Self,
                               m_hSceneDC,
                               RenderSurface.GLContext,
                               Renderer,
                               Shader,
                               pGroup,
                               pModel,
                               textures,
                               matrix,
                               nil,
                               nil);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLShapeGL.OnDrawModelItem(const pGroup: TQRModelGroup;
                                        const pModel: TQRModel;
                                      const textures: TQRTextures;
                                        const matrix: TQRMatrix4x4;
                                         const pMesh: PQRMesh;
                                     const pAABBTree: TQRAABBTree);
begin
    // notify user that model item is about to be drawn on the scene, stop drawing if user already
    // processed it
    if ((not(csDesigning in ComponentState)) and Assigned(OnDrawSceneStaticModelItem)) then
        if (OnDrawSceneStaticModelItem(Self,
                                       m_hSceneDC,
                                       RenderSurface.GLContext,
                                       Renderer,
                                       Shader,
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
    Renderer.Draw(pMesh^, matrix, textures);

    // notify user that collisions may be detected
    if (Assigned(OnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
        OnDetectCollisions(Self,
                           ProjectionMatrix^,
                           ViewMatrix^,
                           matrix,
                           pAABBTree,
                           Renderer,
                           Shader);
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
// TQRVCLSurfaceGL
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRVCLSurfaceGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLSurfaceGL.SetSurfaceWidth(width: Single);
begin
    // no changes?
    if (m_SurfaceWidth = width) then
        Exit;

    m_SurfaceWidth := width;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLSurfaceGL.SetSurfaceHeight(height: Single);
begin
    // no changes?
    if (m_SurfaceHeight = height) then
        Exit;

    m_SurfaceHeight := height;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLSurfaceGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        Exit(False);

    // is OpenGL context created?
    if (RenderSurface.GLContext = 0) then
        Exit(False);

    // is model a surface?
    if (not (m_pShape is TQRSurfaceGroup)) then
        Exit(False);

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
            Exit(False);
    end;

    SetModelLoaded(False);

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pShape);

    // load model
    Result := TQRSurfaceGroup(m_pShape).Load(m_SurfaceWidth,
                                             m_SurfaceHeight,
                                             m_pModel.Color.NativeColor,
                                             m_ModelOptions);
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
// TQRVCLBoxGL
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRVCLBoxGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLBoxGL.SetBoxWidth(width: Single);
begin
    // no changes?
    if (m_BoxWidth = width) then
        Exit;

    m_BoxWidth := width;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLBoxGL.SetBoxHeight(height: Single);
begin
    // no changes?
    if (m_BoxHeight = height) then
        Exit;

    m_BoxHeight := height;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLBoxGL.SetBoxDepth(depth: Single);
begin
    // no changes?
    if (m_BoxDepth = depth) then
        Exit;

    m_BoxDepth := depth;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLBoxGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        Exit(False);

    // is OpenGL context created?
    if (RenderSurface.GLContext = 0) then
        Exit(False);

    // is model a box?
    if (not (m_pShape is TQRBoxGroup)) then
        Exit(False);

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
            Exit(False);
    end;

    SetModelLoaded(False);

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
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
// TQRVCLSphereGL
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRVCLSphereGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLSphereGL.SetSlices(slices: NativeUInt);
begin
    // no changes?
    if (m_Slices = slices) then
        Exit;

    m_Slices := slices;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLSphereGL.SetStacks(stacks: NativeUInt);
begin
    // no changes?
    if (m_Stacks = stacks) then
        Exit;

    m_Stacks := stacks;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLSphereGL.SetRadius(radius: Single);
begin
    // no changes?
    if (m_Radius = radius) then
        Exit;

    m_Radius := radius;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLSphereGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        Exit(False);

    // is OpenGL context created?
    if (RenderSurface.GLContext = 0) then
        Exit(False);

    // is model a sphere?
    if (not (m_pShape is TQRSphereGroup)) then
        Exit(False);

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
            Exit(False);
    end;

    SetModelLoaded(False);

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pShape);

    // load model
    Result := TQRSphereGroup(m_pShape).Load(m_Slices,
                                            m_Stacks,
                                            m_Radius,
                                            m_pModel.Color.NativeColor,
                                            m_ModelOptions);
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
// TQRVCLConeGL
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRVCLConeGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetFaces(faces: NativeUInt);
begin
    // no changes?
    if (m_Faces = faces) then
        Exit;

    m_Faces := faces;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetConeHeight(height: Single);
begin
    // no changes?
    if (m_ConeHeight = height) then
        Exit;

    m_ConeHeight := height;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetTopRadiusX(topRadiusX: Single);
begin
    // no changes?
    if (m_TopRadiusX = topRadiusX) then
        Exit;

    m_TopRadiusX := topRadiusX;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetTopRadiusY(topRadiusY: Single);
begin
    // no changes?
    if (m_TopRadiusY = topRadiusY) then
        Exit;

    m_TopRadiusY := topRadiusY;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetBottomRadiusX(bottomRadiusX: Single);
begin
    // no changes?
    if (m_BottomRadiusX = bottomRadiusX) then
        Exit;

    m_BottomRadiusX := bottomRadiusX;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetBottomRadiusY(bottomRadiusY: Single);
begin
    // no changes?
    if (m_BottomRadiusY = bottomRadiusY) then
        Exit;

    m_BottomRadiusY := bottomRadiusY;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLConeGL.SetClosing(closing: EQR_Cone_Closing);
begin
    // no changes?
    if (m_Closing = closing) then
        Exit;

    m_Closing := closing;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLConeGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        Exit(False);

    // is OpenGL context created?
    if (RenderSurface.GLContext = 0) then
        Exit(False);

    // is model a cone?
    if (not (m_pShape is TQRConeGroup)) then
        Exit(False);

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
            Exit(False);
    end;

    SetModelLoaded(False);

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
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
// TQRVCLTorusGL
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRVCLTorusGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetSlices(slices: NativeUInt);
begin
    // no changes?
    if (m_Slices = slices) then
        Exit;

    m_Slices := slices;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetFacesPerSlices(facesPerSlices: NativeUInt);
begin
    // no changes?
    if (m_FacesPerSlices = facesPerSlices) then
        Exit;

    m_FacesPerSlices := facesPerSlices;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetInnerRadiusX(innerRadiusX: Single);
begin
    // no changes?
    if (m_InnerRadiusX = innerRadiusX) then
        Exit;

    m_InnerRadiusX := innerRadiusX;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetInnerRadiusY(innerRadiusY: Single);
begin
    // no changes?
    if (m_InnerRadiusY = innerRadiusY) then
        Exit;

    m_InnerRadiusY := innerRadiusY;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetOuterRadiusX(outerRadiusX: Single);
begin
    // no changes?
    if (m_OuterRadiusX = outerRadiusX) then
        Exit;

    m_OuterRadiusX := outerRadiusX;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLTorusGL.SetOuterRadiusY(outerRadiusY: Single);
begin
    // no changes?
    if (m_OuterRadiusY = outerRadiusY) then
        Exit;

    m_OuterRadiusY := outerRadiusY;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLTorusGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        Exit(False);

    // is OpenGL context created?
    if (RenderSurface.GLContext = 0) then
        Exit(False);

    // is model a sphere?
    if (not (m_pShape is TQRTorusGroup)) then
        Exit(False);

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
            Exit(False);
    end;

    SetModelLoaded(False);

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
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
// TQRVCLParabolaGL
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRVCLParabolaGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLParabolaGL.SetSlices(slices: NativeUInt);
begin
    // no changes?
    if (m_Slices = slices) then
        Exit;

    m_Slices := slices;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLParabolaGL.SetFacesPerSlices(facesPerSlices: NativeUInt);
begin
    // no changes?
    if (m_FacesPerSlices = facesPerSlices) then
        Exit;

    m_FacesPerSlices := facesPerSlices;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLParabolaGL.SetParabolaHeight(height: Single);
begin
    // no changes?
    if (m_ParabolaHeight = height) then
        Exit;

    m_ParabolaHeight := height;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLParabolaGL.SetRadius(radius: Single);
begin
    // no changes?
    if (m_Radius = radius) then
        Exit;

    m_Radius := Radius;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLParabolaGL.LoadModel: Boolean;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        Exit(False);

    // is OpenGL context created?
    if (RenderSurface.GLContext = 0) then
        Exit(False);

    // is model a parabola?
    if (not (m_pShape is TQRParabolaGroup)) then
        Exit(False);

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
            Exit(False);
    end;

    SetModelLoaded(False);

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
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------

end.
