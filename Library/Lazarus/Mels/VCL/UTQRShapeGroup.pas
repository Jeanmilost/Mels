// *************************************************************************************************
// * ==> UTQRShapeGroup ---------------------------------------------------------------------------*
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
 @abstract(@name provides the features to load and link all shape models and files together.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRShapeGroup;

{$MODE Delphi}

interface

uses Classes,
     Math,
     SysUtils,
     Graphics,
     UTQRCommon,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRShapes,
     UTQRModelGroup,
     UTQRThreading;

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
             Adds vertex to a vertex buffer
             @param(pPosition - vertex possition in space 3D coordinates)
             @param(pNormal - vertex normal)
             @param(pTexCoord - vertex texture coordinates)
             @param(pColor - vertex color)
             @param(index @bold([in, out]) Vertex index in the buffer)
             @param(vertex @bold([in, out]) Vertex info containing buffer in which vertex should be added)
            }
            {$ENDREGION}
            class procedure AddVertex(const pPosition, pNormal: PQRVector3D;
                                               const pTexCoord: PQRVector2D;
                                                  const pColor: TQRColor;
                                                     var index: NativeUInt;
                                                    var vertex: TQRVertex); static;
    end;

    {$REGION 'Documentation'}
    {**
     Generic 3D shape job
    }
    {$ENDREGION}
    TQRShapeJob = class(TQRModelJob)
        private
            m_pModel:         TQRShapeModel;
            m_Textures:       TQRTextures;
            m_pColor:         TQRColor;
            m_MaxTexture:     NativeUInt;
            m_TextureLoaded:  Boolean;
            m_IsCanceled:     Boolean;
            m_fOnLoadTexture: TQRLoadMeshTextureEvent;

        protected
            {$REGION 'Documentation'}
            {**
             Gets model
             @return(Model)
            }
            {$ENDREGION}
            function GetModel: TQRShapeModel; virtual;

            {$REGION 'Documentation'}
            {**
             Gets texture
             @param(index Texture index to get)
             @return(Texture)
            }
            {$ENDREGION}
            function GetTexture(index: NativeInt): TQRTexture; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the texture count
             @return(The texture count)
            }
            {$ENDREGION}
            function GetTextureCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the model color
             @return(The model color)
            }
            {$ENDREGION}
            function GetColor: TQRColor; virtual;

            {$REGION 'Documentation'}
            {**
             Called when the model texture should be loaded
             @br @bold(NOTE) This function is executed on the calling thread side
            }
            {$ENDREGION}
            procedure OnLoadTexture; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Processes the job
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Process: Boolean; override;

            {$REGION 'Documentation'}
            {**
             Cancels the job
            }
            {$ENDREGION}
            procedure Cancel; override;

            {$REGION 'Documentation'}
            {**
             Checks if job was canceled
             @return(@true if job was canceled, otherwise @false)
            }
            {$ENDREGION}
            function IsCanceled: Boolean; virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the model
            }
            {$ENDREGION}
            property Model: TQRShapeModel read GetModel;

            {$REGION 'Documentation'}
            {**
             Gets the model texture at index
            }
            {$ENDREGION}
            property Texture[index: NativeInt]: TQRTexture read GetTexture;

            {$REGION 'Documentation'}
            {**
             Gets the model texture count
            }
            {$ENDREGION}
            property TextureCount: NativeInt read GetTextureCount;

            {$REGION 'Documentation'}
            {**
             Gets the model color
            }
            {$ENDREGION}
            property Color: TQRColor read GetColor;
    end;

    {$REGION 'Documentation'}
    {**
     Generic 3D shape group, contains all items and functions needed to manage a complete shape
    }
    {$ENDREGION}
    TQRShapeGroup = class(TQRStaticModelGroup)
        private
            m_pJob: TQRShapeJob;

        protected
            {$REGION 'Documentation'}
            {**
             Gets dynamic mesh
             @param(mesh @bold([out]) Mesh)
            }
            {$ENDREGION}
            procedure GetDynamicMesh(out mesh: TQRMesh); virtual;

            {$REGION 'Documentation'}
            {**
             Gets dynamic mesh, from cache if available, otherwise calculates and caches it
             @param(mesh @bold([out]) Mesh)
             @param(pTree @bold([out]) Aligned-axis bounding box tree matching with mesh)
            }
            {$ENDREGION}
            procedure GetDynamicMeshUseCache(out pMesh: PQRMesh; out pTree: TQRAABBTree); virtual;

            {$REGION 'Documentation'}
            {**
             Draws dynamic model
            }
            {$ENDREGION}
            procedure DrawDynamicModel; virtual;

            {$REGION 'Documentation'}
            {**
             Draws cached model
            }
            {$ENDREGION}
            procedure DrawCachedModel; virtual;

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
             Clears script
            }
            {$ENDREGION}
            procedure Clear; override;

            {$REGION 'Documentation'}
            {**
             Checks if group is empty
             @return(@true if model is empty, otherwise @false)
            }
            {$ENDREGION}
            function IsEmpty: Boolean; override;

            {$REGION 'Documentation'}
            {**
             Queries the job status
             @return(Job status)
            }
            {$ENDREGION}
            function QueryJobStatus: TQRModelJobStatus; override;

            {$REGION 'Documentation'}
            {**
             Draws group
             @param(elapsedTime Elapsed time since last draw)
            }
            {$ENDREGION}
            procedure Draw(const elapsedTime: Double); override;
    end;

    {$REGION 'Documentation'}
    {**
     Surface job
    }
    {$ENDREGION}
    TQRSurfaceJob = class(TQRShapeJob)
        protected
            {$REGION 'Documentation'}
            {**
             Called before a texture is loaded
             @param(pTexture Texture to load)
             @param(custom If @true, texture is a custom texture provided by the user, otherwise a
                    texture belonging to model)
            }
            {$ENDREGION}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {$REGION 'Documentation'}
            {**
             Called when a known texture should be loaded
             @param(pTexture Texture to load)
             @param(pBitmap Bitmap containing loaded texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Graphics.TBitmap): Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(lengthX Surface length on x axis)
             @param(lengthY Surface length on y axis)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                     lengthX, lengthY: Single;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;
    end;

    {$REGION 'Documentation'}
    {**
     Surface group, contains all items and functions needed to manage a complete surface model
    }
    {$ENDREGION}
    TQRSurfaceGroup = class(TQRShapeGroup)
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
             Loads surface group, creates and initializes model
             @param(lengthX Surface length on x axis)
             @param(lengthY Surface length on y axis)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(lengthX, lengthY: Single;
                              const pColor: TQRColor;
                              modelOptions: TQRModelOptions): Boolean; virtual;
    end;

    {$REGION 'Documentation'}
    {**
     Box job
    }
    {$ENDREGION}
    TQRBoxJob = class(TQRShapeJob)
        protected
            {$REGION 'Documentation'}
            {**
             Called before a texture is loaded
             @param(pTexture Texture to load)
             @param(custom If @true, texture is a custom texture provided by the user, otherwise a
                    texture belonging to model)
            }
            {$ENDREGION}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {$REGION 'Documentation'}
            {**
             Called when a known texture should be loaded
             @param(pTexture Texture to load)
             @param(pBitmap Bitmap containing loaded texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Graphics.TBitmap): Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(lengthX Box length on x axis)
             @param(lengthY Box length on y axis)
             @param(lengthZ Box length on z axis)
             @param(pColor Model color)
             @param(repeatTexOnEachFace If @true, texture will be repeated on each face)
             @param(modelOptions Model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
            lengthX, lengthY, lengthZ: Single;
                         const pColor: TQRColor;
                  repeatTexOnEachFace: Boolean;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;
    end;

    {$REGION 'Documentation'}
    {**
     Box group, contains all items and functions needed to manage a complete box model
    }
    {$ENDREGION}
    TQRBoxGroup = class(TQRShapeGroup)
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
             Loads box group, creates and initializes model
             @param(lengthX Box length on x axis)
             @param(lengthY Box length on y axis)
             @param(lengthZ Box length on z axis)
             @param(pColor Model color)
             @param(repeatTexOnEachFace If @true, texture will be repeated on each face)
             @param(modelOptions Model options to apply)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(lengthX, lengthY, lengthZ: Single;
                                       const pColor: TQRColor;
                                repeatTexOnEachFace: Boolean;
                                       modelOptions: TQRModelOptions): Boolean; virtual;
    end;

    {$REGION 'Documentation'}
    {**
     Sphere job
    }
    {$ENDREGION}
    TQRSphereJob = class(TQRShapeJob)
        protected
            {$REGION 'Documentation'}
            {**
             Called before a texture is loaded
             @param(pTexture Texture to load)
             @param(custom If @true, texture is a custom texture provided by the user, otherwise a
                    texture belonging to model)
            }
            {$ENDREGION}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {$REGION 'Documentation'}
            {**
             Called when a known texture should be loaded
             @param(pTexture Texture to load)
             @param(pBitmap Bitmap containing loaded texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Graphics.TBitmap): Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(slices Sphere slices to generate)
             @param(stacks Sphere stacks to generate)
             @param(radius Sphere radius)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                       slices, stacks: NativeUInt;
                               radius: Single;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;
    end;

    {$REGION 'Documentation'}
    {**
     Sphere group, contains all items and functions needed to manage a complete sphere model
    }
    {$ENDREGION}
    TQRSphereGroup = class(TQRShapeGroup)
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
             Loads sphere group, creates and initializes model
             @param(slices Sphere slices to generate)
             @param(stacks Sphere stacks to generate)
             @param(radius Sphere radius)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(slices, stacks: NativeUInt;
                                  radius: Single;
                            const pColor: TQRColor;
                            modelOptions: TQRModelOptions): Boolean; virtual;
    end;

    {$REGION 'Documentation'}
    {**
     Cone job
    }
    {$ENDREGION}
    TQRConeJob = class(TQRShapeJob)
        protected
            {$REGION 'Documentation'}
            {**
             Called before a texture is loaded
             @param(pTexture Texture to load)
             @param(custom If @true, texture is a custom texture provided by the user, otherwise a
                    texture belonging to model)
            }
            {$ENDREGION}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {$REGION 'Documentation'}
            {**
             Called when a known texture should be loaded
             @param(pTexture Texture to load)
             @param(pBitmap Bitmap containing loaded texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Graphics.TBitmap): Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(faceCount Number of faces composing the cone body (without top and bottom closing)
             @param(height Cone height)
             @param(topRadiusX x axis radius of the ellipse formed by the truncated apex, the cone
                               is not truncated if equals to 0.0)
             @param(topRadiusY y axis radius of the ellipse formed by the truncated apex, ignored if
                               topRadiusX is equals to 0.0)
             @param(bottomRadiusX x axis radius of the ellipse formed by the base)
             @param(bottomRadiusY y axis radius of the ellipse formed by the base)
             @param(closing Cone closing type)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
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

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;
    end;

    {$REGION 'Documentation'}
    {**
     Cone group, contains all items and functions needed to manage a complete cone model
    }
    {$ENDREGION}
    TQRConeGroup = class(TQRShapeGroup)
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
             Loads cone group, creates and initializes model
             @param(faceCount Number of faces composing the cone body (without top and bottom closing))
             @param(height Cone height)
             @param(topRadiusX x axis radius of the ellipse formed by the truncated apex, the cone
                               is not truncated if equals to 0.0)
             @param(topRadiusY y axis radius of the ellipse formed by the truncated apex, ignored if
                               topRadiusX is equals to 0.0)
             @param(bottomRadiusX x axis radius of the ellipse formed by the base)
             @param(bottomRadiusY y axis radius of the ellipse formed by the base)
             @param(closing Cone closing type)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
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

    {$REGION 'Documentation'}
    {**
     Torus job
    }
    {$ENDREGION}
    TQRTorusJob = class(TQRShapeJob)
        protected
            {$REGION 'Documentation'}
            {**
             Called before a texture is loaded
             @param(pTexture Texture to load)
             @param(custom If @true, texture is a custom texture provided by the user, otherwise a
                    texture belonging to model)
            }
            {$ENDREGION}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {$REGION 'Documentation'}
            {**
             Called when a known texture should be loaded
             @param(pTexture Texture to load)
             @param(pBitmap Bitmap containing loaded texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Graphics.TBitmap): Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(slices Torus slices)
             @param(facesPerSlices For each torus slice, the number of faces to build)
             @param(outerRadiusX Outer radius on the (local) x axis)
             @param(outerRadiusY Outer radius on the (local) y axis)
             @param(innerRadiusX Inner radius on the (local) x axis)
             @param(innerRadiusY Inner radius on the (local) y axis)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
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

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;
    end;

    {$REGION 'Documentation'}
    {**
     Torus group, contains all items and functions needed to manage a complete torus model
    }
    {$ENDREGION}
    TQRTorusGroup = class(TQRShapeGroup)
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
             Loads cone group, creates and initializes model
             @param(slices Torus slices)
             @param(facesPerSlices For each torus slice, the number of faces to build)
             @param(outerRadiusX Outer radius on the (local) x axis)
             @param(outerRadiusY Outer radius on the (local) y axis)
             @param(innerRadiusX Inner radius on the (local) x axis)
             @param(innerRadiusY Inner radius on the (local) y axis)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(slices,
                  facesPerSlices: NativeUInt;
                    outerRadiusX,
                    outerRadiusY,
                    innerRadiusX,
                    innerRadiusY: Single;
                    const pColor: TQRColor;
                    modelOptions: TQRModelOptions): Boolean; virtual;
    end;

    {$REGION 'Documentation'}
    {**
     Parabola job
    }
    {$ENDREGION}
    TQRParabolaJob = class(TQRShapeJob)
        protected
            {$REGION 'Documentation'}
            {**
             Called before a texture is loaded
             @param(pTexture Texture to load)
             @param(custom If @true, texture is a custom texture provided by the user, otherwise a
                    texture belonging to model)
            }
            {$ENDREGION}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {$REGION 'Documentation'}
            {**
             Called when a known texture should be loaded
             @param(pTexture Texture to load)
             @param(pBitmap Bitmap containing loaded texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Graphics.TBitmap): Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(slices Parabola slices)
             @param(facesPerSlices For each parabola slice, the number of faces to build)
             @param(height Parabola height)
             @param(radius Parabola radius)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                               slices,
                       facesPerSlices: NativeUInt;
                       height, radius: Single;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;
    end;

    {$REGION 'Documentation'}
    {**
     Parabola group, contains all items and functions needed to manage a complete parabola model
    }
    {$ENDREGION}
    TQRParabolaGroup = class(TQRShapeGroup)
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
             Loads cone group, creates and initializes model
             @param(slices Parabola slices)
             @param(facesPerSlices For each parabola slice, the number of faces to build)
             @param(height Parabola height)
             @param(radius Parabola radius)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(slices, facesPerSlices: NativeUInt;
                                  height, radius: Single;
                                    const pColor: TQRColor;
                                    modelOptions: TQRModelOptions): Boolean; virtual;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRShapeHelper
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
// TQRShapeJob
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRShapeJob.Destroy;
var
    pTexture: TQRTexture;
begin
    m_pLock.Lock;

    try
        // clear textures
        for pTexture in m_Textures do
            pTexture.Free;

        SetLength(m_Textures, 0);

        // clear memory
        m_pModel.Free;
        m_pColor.Free;
    finally
        m_pLock.Unlock;
    end;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRShapeJob.GetModel: TQRShapeModel;
begin
    m_pLock.Lock;
    Result := m_pModel;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRShapeJob.GetTexture(index: NativeInt): TQRTexture;
begin
    m_pLock.Lock;

    if (index >= Length(m_Textures)) then
        Exit(nil);

    Result := m_Textures[index];

    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRShapeJob.GetTextureCount: NativeInt;
begin
    m_pLock.Lock;
    Result := Length(m_Textures);
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRShapeJob.GetColor: TQRColor;
begin
    m_pLock.Lock;
    Result := m_pColor;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShapeJob.OnLoadTexture;
var
    textureIndex: NativeInt;
    max:          NativeUInt;
    loadNext:     Boolean;
begin
    m_pLock.Lock;

    try
        m_TextureLoaded := False;

        if (GetStatus = EQR_JS_Canceled) then
            Exit;

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
                if (not m_fOnLoadTexture(GetGroup,
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
//--------------------------------------------------------------------------------------------------
function TQRShapeJob.Process: Boolean;
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
    if (IsLoaded) then
        Exit(True);

    try
        // check if cache should be created
        doCreateCache := ((EQR_MO_Create_Cache   in ModelOptions) and
                      not (EQR_MO_Dynamic_Frames in ModelOptions));

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
        Progress := Progress + progressStep;

        m_pLock.Lock;
        textureLoaded := m_TextureLoaded;
        m_pLock.Unlock;

        // do include colors?
        if (EQR_MO_Without_Colors in ModelOptions) then
            vertexFormat := []
        else
            vertexFormat := [EQR_VF_Colors];

        // normals loaded?
        if (not(EQR_MO_Without_Normals in ModelOptions)) then
            Include(vertexFormat, EQR_VF_Normals);

        // texture loaded?
        if (textureLoaded and (not(EQR_MO_Without_Textures in ModelOptions))) then
            Include(vertexFormat, EQR_VF_TexCoords);

        // set vertex format
        m_pModel.VertexFormat := vertexFormat;

        // model is configured, add one step to progress
        Progress := Progress + progressStep;

        // do not create cache?
        if (not doCreateCache) then
        begin
            IsLoaded := True;
            Exit(True);
        end;

        // create mesh
        New(pMesh);

        // do ignore collisions?
        if (not(EQR_MO_No_Collision in ModelOptions)) then
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
            Exit(False);
        end;

        // add mesh to cache, note that from now cache will take care of the pointer
        try
            SetMesh(0, pMesh);
        except
            Dispose(pMesh);
        end;

        // do ignore collisions?
        if (not(EQR_MO_No_Collision in ModelOptions)) then
            // add tree to cache, note that from now cache will take care of the pointer
            try
                SetTree(0, pTree);
            except
                pTree.Free;
            end;

        // cache was created, add one step to progress
        Progress := Progress + progressStep;

        IsLoaded := True;
        Result   := True;
    finally
        TThread.Synchronize(nil, OnAfterLoadModel);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShapeJob.Cancel;
begin
    m_pLock.Lock;
    m_IsCanceled := True;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRShapeJob.IsCanceled: Boolean;
begin
    m_pLock.Lock;
    Result := m_IsCanceled;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
// TQRShapeGroup
//--------------------------------------------------------------------------------------------------
constructor TQRShapeGroup.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRShapeGroup.Destroy;
begin
    // delete model and his associated job, don't forget to unregister it from worker
    if (Assigned(m_pJob)) then
    begin
        TQRModelWorker.GetInstance.CancelJob(m_pJob);
        m_pJob := nil;
    end;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShapeGroup.Clear;
begin
    // previous job was created?
    if (Assigned(m_pJob)) then
    begin
        // delete previous job
        TQRModelWorker.GetInstance.CancelJob(m_pJob);
        m_pJob := nil;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRShapeGroup.IsEmpty: Boolean;
begin
    Result := (not Assigned(m_pJob));
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
procedure TQRShapeGroup.DrawDynamicModel;
var
    pMesh: PQRMesh;
    pTree: TQRAABBTree;
begin
    // nothing to draw?
    if (not Assigned(OnDrawItem)) then
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
            OnDrawItem(Self, m_pJob.Model, m_pJob.m_Textures, GetMatrix, pMesh, nil);
        finally
            // clear memory
            Dispose(pMesh);
        end;

        Exit;
    end;

    // get meshes and AABB trees from cache, create them if still not exist
    GetDynamicMeshUseCache(pMesh, pTree);

    // draw mesh
    OnDrawItem(Self, m_pJob.Model, m_pJob.m_Textures, GetMatrix, pMesh, pTree);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShapeGroup.DrawCachedModel;
begin
    // nothing to draw?
    if (not Assigned(OnDrawItem)) then
        Exit;

    // collision buffers were created?
    if (EQR_MO_No_Collision in m_pJob.ModelOptions) then
        // draw mesh, ignore collisions
        OnDrawItem(Self, m_pJob.Model, m_pJob.m_Textures, GetMatrix, m_pJob.Mesh[0], nil)
    else
        // draw mesh
        OnDrawItem(Self,
                   m_pJob.Model,
                   m_pJob.m_Textures,
                   GetMatrix,
                   m_pJob.Mesh[0],
                   m_pJob.AABBTree[0]);
end;
//--------------------------------------------------------------------------------------------------
function TQRShapeGroup.QueryJobStatus: TQRModelJobStatus;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
    begin
        // set default values
        JobStatus.Status   := EQR_JS_NotStarted;
        JobStatus.Progress := 0;
    end
    else
    begin
        // get status from running job
        JobStatus.Status   := m_pJob.GetStatus;
        JobStatus.Progress := Floor(m_pJob.Progress);
    end;

    Result := JobStatus;
end;
//--------------------------------------------------------------------------------------------------
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
        DrawCachedModel
    else
    if ((EQR_MO_Dynamic_Frames          in m_pJob.ModelOptions) or
        (EQR_MO_Dynamic_Frames_No_Cache in m_pJob.ModelOptions))
    then
        DrawDynamicModel
    else
    if (Assigned(OnCustomDrawItem))
    then
        // let user take care of drawing model
        OnCustomDrawItem(Self,
                         m_pJob.Model,
                         m_pJob.m_Textures,
                         GetMatrix);
end;
//--------------------------------------------------------------------------------------------------
// TQRSurfaceJob
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRSurfaceJob.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRSurfaceJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_surface';
end;
//--------------------------------------------------------------------------------------------------
function TQRSurfaceJob.LoadTexture(pTexture: TQRTexture;
                                    pBitmap: Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRSurfaceGroup
//--------------------------------------------------------------------------------------------------
constructor TQRSurfaceGroup.Create;
begin
    inherited Create;

    m_pJob := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRSurfaceGroup.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRSurfaceGroup.Load(lengthX, lengthY: Single;
                                  const pColor: TQRColor;
                                  modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear;

    // prepare model job to load from file
    m_pJob := TQRSurfaceJob.Create(Self,
                                   lengthX,
                                   lengthY,
                                   pColor,
                                   modelOptions,
                                   OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRBoxJob
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRBoxJob.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRBoxJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_box';
end;
//--------------------------------------------------------------------------------------------------
function TQRBoxJob.LoadTexture(pTexture: TQRTexture;
                                pBitmap: Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRBoxGroup
//--------------------------------------------------------------------------------------------------
constructor TQRBoxGroup.Create;
begin
    inherited Create;

    m_pJob := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRBoxGroup.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRBoxGroup.Load(lengthX, lengthY, lengthZ: Single;
                                       const pColor: TQRColor;
                                repeatTexOnEachFace: Boolean;
                                       modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear;

    // prepare model job to load from file
    m_pJob := TQRBoxJob.Create(Self,
                               lengthX,
                               lengthY,
                               lengthZ,
                               pColor,
                               repeatTexOnEachFace,
                               modelOptions,
                               OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRSphereJob
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRSphereJob.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRSphereJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_sphere';
end;
//--------------------------------------------------------------------------------------------------
function TQRSphereJob.LoadTexture(pTexture: TQRTexture;
                                   pBitmap: Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRSphereGroup
//--------------------------------------------------------------------------------------------------
constructor TQRSphereGroup.Create;
begin
    inherited Create;

    m_pJob := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRSphereGroup.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRSphereGroup.Load(slices, stacks: NativeUInt;
                                     radius: Single;
                               const pColor: TQRColor;
                               modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear;

    // prepare model job to load from file
    m_pJob := TQRSphereJob.Create(Self,
                                  slices,
                                  stacks,
                                  radius,
                                  pColor,
                                  modelOptions,
                                  OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRConeJob
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRConeJob.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRConeJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_cone';
end;
//--------------------------------------------------------------------------------------------------
function TQRConeJob.LoadTexture(pTexture: TQRTexture;
                                 pBitmap: Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRConeGroup
//--------------------------------------------------------------------------------------------------
constructor TQRConeGroup.Create;
begin
    inherited Create;

    m_pJob := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRConeGroup.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
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
    Clear;

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
                                OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRTorusJob
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRTorusJob.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRTorusJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_torus';
end;
//--------------------------------------------------------------------------------------------------
function TQRTorusJob.LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRTorusGroup
//--------------------------------------------------------------------------------------------------
constructor TQRTorusGroup.Create;
begin
    inherited Create;

    m_pJob := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRTorusGroup.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
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
    Clear;

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
                                 OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRParabolaJob
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------
destructor TQRParabolaJob.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRParabolaJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    pTexture.Name := 'qr_parabola';
end;
//--------------------------------------------------------------------------------------------------
function TQRParabolaJob.LoadTexture(pTexture: TQRTexture;
                                     pBitmap: Graphics.TBitmap): Boolean;
begin
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRParabolaGroup
//--------------------------------------------------------------------------------------------------
constructor TQRParabolaGroup.Create;
begin
    inherited Create;

    m_pJob := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRParabolaGroup.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRParabolaGroup.Load(slices, facesPerSlices: NativeUInt;
                                       height, radius: Single;
                                         const pColor: TQRColor;
                                         modelOptions: TQRModelOptions): Boolean;
begin
    // clear previous group instance
    Clear;

    // prepare model job to load from file
    m_pJob := TQRParabolaJob.Create(Self,
                                    slices,
                                    facesPerSlices,
                                    height,
                                    radius,
                                    pColor,
                                    modelOptions,
                                    OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------

end.
