// *************************************************************************************************
// * ==> Main -------------------------------------------------------------------------------------*
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
 @abstract(@name contains the 3D geometrical shapes demo main form.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit Main;

interface

uses System.Classes,
     System.SysUtils,
     System.Variants,
     System.UITypes,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.Forms,
     Vcl.Dialogs,
     Vcl.Menus,
     Winapi.Messages,
     Winapi.Windows,
     Winapi.OpenGL,
     Winapi.OpenGLext,
     UTQRSmartPointer,
     UTQRGraphics,
     UTQR3D,
     UTQRGeometry,
     UTQRModel,
     UTQRModelGroup,
     UTQRShapes,
     UTQRShapeGroup,
     UTQROpenGLHelper;

type
    {**
     3D geometrical shapes demo main form
    }
    TMainForm = class(TForm)
        published
            pmOptions: TPopupMenu;
            miLighting: TMenuItem;

            procedure FormCreate(pSender: TObject);
            procedure FormResize(pSender: TObject);
            procedure FormPaint(pSender: TObject);
            procedure miLightingClick(pSender: TObject);

        private
            m_hDC:          THandle;
            m_hRC:          THandle;
            m_pSurface:     TQRSurfaceGroup;
            m_pSphere:      TQRSphereGroup;
            m_pBox:         TQRBoxGroup;
            m_pCone:        TQRConeGroup;
            m_pTorus:       TQRTorusGroup;
            m_pParabola:    TQRParabolaGroup;
            m_pCylinder:    TQRConeGroup;
            m_pPyramid:     TQRConeGroup;
            m_pTetrahedron: TQRSphereGroup;
            m_PreviousTime: NativeUInt;

            {**
             Configures OpenGL
            }
            procedure ConfigOpenGL;

            {**
             Loads texture from resource
             @param(resName Resource name)
             @return(OpenGL texture index, 0 on error or if failed)
            }
            function LoadTexture(resName: PChar): GLint;

            {**
             Called when mesh texture should be loaded
             @param(pModel Model for which texture should be loaded)
             @param(pBitmap Whenever possible, the bitmap containing the texture, @nil if not available)
             @param(pTexture Texture info)
             @param(loadNext @bold([out]) If @true, event will be called again with a new item to
                                          load next texture)
             @return(@true on success, otherwise @false)
            }
            function OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                       const pModel: TQRModel;
                                            pBitmap: Vcl.Graphics.TBitmap;
                                           pTexture: TQRTexture;
                                       out loadNext: Boolean): Boolean;

            {**
             Called when static model will be drawn and caller should extract mesh
             @param(pGroup Group at which model belongs)
             @param(pModel Model to draw)
             @param(textures Texture list, in order they should be linked)
             @param(matrix Model matrix)
            }
            procedure OnDrawCustomStaticModelItem(const pGroup: TQRModelGroup;
                                                        pModel: TQRModel;
                                                const textures: TQRTextures;
                                                  const matrix: TQRMatrix4x4);

        protected
            {**
             Called when application do nothing
             @param(pSender Event sender)
             @param(done @bold([in, out]) If @true, loop will be considered as completed)
            }
            procedure OnIdle(pSender: TObject; var done: Boolean); virtual;

            {**
             Renders (i.e. prepares and draws) scene
            }
            procedure RenderGLScene; virtual;

            {**
             Draws scene
             @param(elapsedTime Elapsed time since last draw)
            }
            procedure Draw(const elapsedTime: Double);

        public
            {**
             Constructor
             @param(pOwner Form owner)
            }
            constructor Create(pOwner: TComponent); override;

            {**
             Destructor
            }
            destructor Destroy; override;
    end;

var
  MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_hDC          := 0;
    m_hRC          := 0;
    m_pSurface     := nil;
    m_pSphere      := nil;
    m_pBox         := nil;
    m_pCone        := nil;
    m_pTorus       := nil;
    m_pParabola    := nil;
    m_pCylinder    := nil;
    m_pPyramid     := nil;
    m_pTetrahedron := nil;
    m_PreviousTime := GetTickCount;

    miLighting.Checked := True;
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.Destroy;
begin
    // delete surface
    m_pSurface.Free;

    // delete sphere
    m_pSphere.Free;

    // delete box
    m_pBox.Free;

    // delete cone
    m_pCone.Free;

    // delete torus
    m_pTorus.Free;

    // delete parabola
    m_pParabola.Free;

    // delete cylinder
    m_pCylinder.Free;

    // delete pyramid
    m_pPyramid.Free;

    // delete tetrahedron
    m_pTetrahedron.Free;

    // shutdown OpenGL
    TQROpenGLHelper.DisableOpenGL(Handle, m_hDC, m_hRC);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormCreate(pSender: TObject);
var
    pSurfaceColor,
    pSphereColor,
    pBoxColor,
    pConeColor,
    pTorusColor,
    pParabolaColor,
    pCylinderColor,
    pPyramidColor,
    pTetrahedronColor: IQRSmartPointer<TQRColor>;
begin
    // initialize OpenGL
    if (not TQROpenGLHelper.EnableOpenGL(Handle, m_hDC, m_hRC)) then
    begin
        MessageDlg('OpenGL could not be initialized.' + #13#10#13#10 + 'Application will close.',
                   mtError,
                   [mbOK],
                   0);

        Application.Terminate;
        Exit;
    end;

    // configure OpenGL
    ConfigOpenGL;

    // create and configure surface
    m_pSurface                   := TQRSurfaceGroup.Create;
    m_pSurface.OnLoadMeshTexture := OnLoadMeshTexture;
    m_pSurface.OnCustomDrawItem  := OnDrawCustomStaticModelItem;

    pSurfaceColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(255, 255, 255, 255));

    // create and configure sphere
    m_pSurface.OnCustomDrawItem := OnDrawCustomStaticModelItem;

    // load sphere model
    m_pSurface.Load(0.15, 0.15, pSurfaceColor, [EQR_MO_Without_Colors]);

    // locate model in world
    m_pSurface.Translation^ := TQRVector3D.Create(-0.2, 0.2, -1.0);

    // create and configure sphere
    m_pSphere                   := TQRSphereGroup.Create;
    m_pSphere.OnLoadMeshTexture := OnLoadMeshTexture;
    m_pSphere.OnCustomDrawItem  := OnDrawCustomStaticModelItem;

    pSphereColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(255, 255, 255, 255));

    // create and configure sphere
    m_pSphere.OnCustomDrawItem := OnDrawCustomStaticModelItem;

    // load sphere model
    m_pSphere.Load(20, 20, 0.09, pSphereColor, [EQR_MO_Without_Colors]);

    // locate model in world
    m_pSphere.Translation^ := TQRVector3D.Create(0.0, 0.2, -1.0);
    m_pSphere.RotationX    := -PI / 2.0;

    // create and configure box
    m_pBox                   := TQRBoxGroup.Create;
    m_pBox.OnCustomDrawItem  := OnDrawCustomStaticModelItem;
    m_pBox.OnLoadMeshTexture := OnLoadMeshTexture;

    pBoxColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(255, 255, 255, 255));

    // load box model
    m_pBox.Load(0.11, 0.11, 0.11, pBoxColor, False, [EQR_MO_Without_Colors]);

    // locate model in world
    m_pBox.Translation^ := TQRVector3D.Create(0.2, 0.2, -1.0);
    m_pBox.RotationX    := PI / 5.0;
    m_pBox.RotationY    := PI + (PI / 3.0);

    // create and configure cone
    m_pCone                   := TQRConeGroup.Create;
    m_pCone.OnCustomDrawItem  := OnDrawCustomStaticModelItem;
    m_pCone.OnLoadMeshTexture := OnLoadMeshTexture;

    pConeColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(255, 255, 255, 255));

    // load cone model
    m_pCone.Load(20, 0.15, 0.055, 0.055, 0.1, 0.1, EQR_CC_Both, pConeColor, [EQR_MO_Without_Colors]);

    // locate model in world
    m_pCone.Translation^ := TQRVector3D.Create(-0.2, 0.0, -1.0);

    // create and configure torus
    m_pTorus                   := TQRTorusGroup.Create;
    m_pTorus.OnCustomDrawItem  := OnDrawCustomStaticModelItem;
    m_pTorus.OnLoadMeshTexture := OnLoadMeshTexture;

    pTorusColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(255, 255, 255, 255));

    // load torus model
    m_pTorus.Load(20, 20, 0.075, 0.075, 0.025, 0.025, pTorusColor, [EQR_MO_Without_Colors]);

    // locate model in world
    m_pTorus.Translation^ := TQRVector3D.Create(0.0, 0.0, -1.0);
    m_pTorus.RotationX    := PI / 1.5;
    m_pTorus.RotationY    := PI / 5.0;

    // create and configure parabola
    m_pParabola                   := TQRParabolaGroup.Create;
    m_pParabola.OnCustomDrawItem  := OnDrawCustomStaticModelItem;
    m_pParabola.OnLoadMeshTexture := OnLoadMeshTexture;

    pParabolaColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(255, 255, 255, 255));

    // load parabola model
    m_pParabola.Load(20, 20, 0.04, 0.011, pParabolaColor, [EQR_MO_Without_Colors]);

    // locate model in world
    m_pParabola.Translation^ :=  TQRVector3D.Create(0.22, -0.03, -1.0);
    m_pParabola.RotationX    :=  PI + (PI / 1.5);
    m_pParabola.RotationY    := -PI / 5.0;

    // create and configure cylinder
    m_pCylinder                   := TQRConeGroup.Create;
    m_pCylinder.OnCustomDrawItem  := OnDrawCustomStaticModelItem;
    m_pCylinder.OnLoadMeshTexture := OnLoadMeshTexture;

    pCylinderColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(255, 255, 255, 255));

    // load cylinder model (it's a cone where top and bottom radius are equals)
    m_pCylinder.Load(20,
                     0.15,
                     0.06,
                     0.06,
                     0.06,
                     0.06,
                     EQR_CC_Both,
                     pCylinderColor,
                     [EQR_MO_Without_Colors]);

    // locate model in world
    m_pCylinder.Translation^ := TQRVector3D.Create(-0.2, -0.2, -1.0);
    m_pCylinder.RotationY    := PI / 1.5;

    // create and configure pyramid
    m_pPyramid                   := TQRConeGroup.Create;
    m_pPyramid.OnCustomDrawItem  := OnDrawCustomStaticModelItem;
    m_pPyramid.OnLoadMeshTexture := OnLoadMeshTexture;

    pPyramidColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(255, 255, 255, 255));

    // load pyramid model (it's a cone with few faces)
    m_pPyramid.Load(4,
                    0.15,
                    0.001, // needed, otherwise the normals will not be generated correctly
                    0.001,
                    0.12,
                    0.12,
                    EQR_CC_Both,
                    pPyramidColor,
                    [EQR_MO_Without_Colors]);

    // locate model in world
    m_pPyramid.Translation^ := TQRVector3D.Create(0.0, -0.2, -1.0);

    // create and configure tetrahedron
    m_pTetrahedron                   := TQRSphereGroup.Create;
    m_pTetrahedron.OnCustomDrawItem  := OnDrawCustomStaticModelItem;
    m_pTetrahedron.OnLoadMeshTexture := OnLoadMeshTexture;

    pTetrahedronColor := TQRSmartPointer<TQRColor>.Create(TQRColor.Create(255, 255, 255, 255));

    // load tetrahedron model (it's a sphere with few faces)
    m_pTetrahedron.Load(4, 5, 0.09, pTetrahedronColor, [EQR_MO_Without_Colors]);

    // locate model in world
    m_pTetrahedron.Translation^ := TQRVector3D.Create(0.2, -0.2, -1.0);
    m_pTetrahedron.RotationY    := PI / 1.5;

    // from now, OpenGL will draw scene every time the thread do nothing else
    Application.OnIdle := OnIdle;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormResize(pSender: TObject);
begin
    TQROpenGLHelper.CreateViewport(ClientWidth, ClientHeight, True);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormPaint(pSender: TObject);
begin
    RenderGLScene;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.miLightingClick(pSender: TObject);
begin
    // toggle light menu item
    miLighting.Checked := not miLighting.Checked;

    // enable or disable lighting
    if (miLighting.Checked) then
        glEnable(GL_LIGHTING)
    else
        glDisable(GL_LIGHTING);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ConfigOpenGL;
const
    lightpos: array [0..3] of GLfloat = (0.5, 0.0, 0.5, 0.0);
    color:    array [0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
begin
    // configure OpenGL
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glEnable(GL_TEXTURE_2D);

    // enable lighting
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

    // set light direction (setting a position with a w value equals to 0 means a direction)
    glLightfv(GL_LIGHT0, GL_POSITION, PGLfloat(@lightpos[0]));

    // set light color to white
    glMaterialfv(GL_BACK, GL_DIFFUSE, PGLfloat(@color[0]));
end;
//--------------------------------------------------------------------------------------------------
function TMainForm.LoadTexture(resName: PChar): GLint;
var
    hPackageInstance: THandle;
    pTextureStream:   TResourceStream;
    pBitmap:          Vcl.Graphics.TBitmap;
    pixelFormat:      Integer;
    pPixels:          PByte;
begin
    // get module instance at which this form belongs
    hPackageInstance := FindClassHInstance(TMainForm);

    // found it?
    if (hPackageInstance = 0) then
        Exit(-1);

    pTextureStream := nil;
    pBitmap        := nil;

    try
        // load resources
        if (FindResource(hPackageInstance, resName, RT_RCDATA) <> 0) then
            pTextureStream := TResourceStream.Create(hPackageInstance, resName, RT_RCDATA)
        else
            Exit(-1);

        // load box texture
        pBitmap := Vcl.Graphics.TBitmap.Create;
        pBitmap.LoadFromStream(pTextureStream);

        if (pBitmap.PixelFormat = pf32bit) then
            pixelFormat := GL_RGBA
        else
            pixelFormat := GL_RGB;

        Result  := -1;
        pPixels := nil;

        try
            // convert bitmap to pixel array, and create OpenGL texture from array
            TQROpenGLHelper.BytesFromBitmap(pBitmap, pPixels, False, False);
            Result := TQROpenGLHelper.CreateTexture(pBitmap.Width,
                                                    pBitmap.Height,
                                                    pixelFormat,
                                                    pPixels,
                                                    GL_NEAREST,
                                                    GL_NEAREST,
                                                    GL_TEXTURE_2D);
        finally
            if (Assigned(pPixels)) then
                FreeMem(pPixels);
        end;
    finally
        pBitmap.Free;
        pTextureStream.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TMainForm.OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                     const pModel: TQRModel;
                                          pBitmap: Vcl.Graphics.TBitmap;
                                         pTexture: TQRTexture;
                                     out loadNext: Boolean): Boolean;
begin
    // no model?
    if (not Assigned(pModel)) then
        Exit(False);

    // no texture?
    if (not Assigned(pTexture)) then
        Exit(False);

    // is model a surface?
    if (pModel is TQRSurfaceModel) then
    begin
        pTexture.Index := LoadTexture('ID_SURFACE_TEXTURE');
        Exit(True);
    end;

    // is model a sphere?
    if (pModel is TQRSphereModel) then
    begin
        // select texture to create in relation with model
        if (pGroup = m_pSphere) then
            pTexture.Index := LoadTexture('ID_SPHERE_TEXTURE')
        else
        if (pGroup = m_pTetrahedron) then
            pTexture.Index := LoadTexture('ID_STONE_TEXTURE');

        Exit(True);
    end;

    // is model a box?
    if (pModel is TQRBoxModel) then
    begin
        pTexture.Index := LoadTexture('ID_BOX_TEXTURE');
        Exit(True);
    end;

    // is model a cone?
    if (pModel is TQRConeModel) then
    begin
        // select texture to create in relation with model
        if (pGroup = m_pCone) then
            pTexture.Index := LoadTexture('ID_CONE_TEXTURE')
        else
        if (pGroup = m_pCylinder) then
            pTexture.Index := LoadTexture('ID_CYLINDER_TEXTURE')
        else
        if (pGroup = m_pPyramid) then
            pTexture.Index := LoadTexture('ID_PYRAMID_TEXTURE');

        Exit(True);
    end;

    // is model a torus?
    if (pModel is TQRTorusModel) then
    begin
        pTexture.Index := LoadTexture('ID_TORUS_TEXTURE');
        Exit(True);
    end;

    // is model a parabola?
    if (pModel is TQRParabolaModel) then
    begin
        pTexture.Index := LoadTexture('ID_PARABOLA_TEXTURE');
        Exit(True);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnDrawCustomStaticModelItem(const pGroup: TQRModelGroup;
                                                      pModel: TQRModel;
                                              const textures: TQRTextures;
                                                const matrix: TQRMatrix4x4);
var
    mesh: TQRMesh;
begin
    // no model?
    if (not Assigned(pModel)) then
        Exit;

    // is model a surface?
    if (pModel is TQRSurfaceModel) then
    begin
        // get surface mesh to draw
        TQRSurfaceModel(pModel).GetMesh(mesh, nil);

        // draw mesh
        TQROpenGLHelper.Draw(mesh, matrix, textures);

        Exit;
    end;

    // is model a sphere?
    if (pModel is TQRSphereModel) then
    begin
        // get sphere mesh to draw
        TQRSphereModel(pModel).GetMesh(mesh, nil);

        // draw mesh
        TQROpenGLHelper.Draw(mesh, matrix, textures);

        Exit;
    end;

    // is model a box?
    if (pModel is TQRBoxModel) then
    begin
        // get box mesh to draw
        TQRBoxModel(pModel).GetMesh(mesh, nil);

        // draw mesh
        TQROpenGLHelper.Draw(mesh, matrix, textures);

        Exit;
    end;

    // is model a cone?
    if (pModel is TQRConeModel) then
    begin
        // get cone mesh to draw
        TQRConeModel(pModel).GetMesh(mesh, nil);

        // draw mesh
        TQROpenGLHelper.Draw(mesh, matrix, textures);

        Exit;
    end;

    // is model a torus?
    if (pModel is TQRTorusModel) then
    begin
        // get torus mesh to draw
        TQRTorusModel(pModel).GetMesh(mesh, nil);

        // draw mesh
        TQROpenGLHelper.Draw(mesh, matrix, textures);

        Exit;
    end;

    // is model a parabola?
    if (pModel is TQRParabolaModel) then
    begin
        // get torus mesh to draw
        TQRParabolaModel(pModel).GetMesh(mesh, nil);

        // draw mesh
        TQROpenGLHelper.Draw(mesh, matrix, textures);

        Exit;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnIdle(pSender: TObject; var done: Boolean);
begin
    done := False;
    RenderGLScene;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.RenderGLScene;
var
    now:         NativeUInt;
    elapsedTime: Double;
begin
    if ((m_hDC = 0) or (m_hRC = 0)) then
        Exit;

    wglMakeCurrent(m_hDC, m_hRC);

    // calculate time interval
    now            :=  GetTickCount;
    elapsedTime    := (now - m_PreviousTime);
    m_PreviousTime :=  now;

    // clear scene
    glClearColor(0.0, 0.0, 0.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // draw scene
    Draw(elapsedTime);

    glFlush;

    // finalize scene
    SwapBuffers(m_hDC);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.Draw(const elapsedTime: Double);
begin
    // draw models
    m_pSurface.Draw(elapsedTime);
    m_pSphere.Draw(elapsedTime);
    m_pBox.Draw(elapsedTime);
    m_pCone.Draw(elapsedTime);
    m_pTorus.Draw(elapsedTime);
    m_pParabola.Draw(elapsedTime);
    m_pCylinder.Draw(elapsedTime);
    m_pPyramid.Draw(elapsedTime);
    m_pTetrahedron.Draw(elapsedTime);
end;
//--------------------------------------------------------------------------------------------------

end.
