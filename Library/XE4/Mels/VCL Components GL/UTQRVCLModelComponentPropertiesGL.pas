{**************************************************************************************************
 * ==> UTQRVCLModelComponentPropertiesGL ---------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides common properties that can be exposed by a model component  *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLModelComponentPropertiesGL;

interface

uses System.Classes,
     System.SysUtils,
     UTQRGeometry,
     UTQR3D,
     UTQRModelGroup,
     UTQRVCLGraphics,
     UTQRMD2,
     Vcl.Graphics,
     Vcl.Controls,
     Winapi.Windows;

type
    {**
    * Messages a property can use to notify his owner
    *}
    EQRPropMessages =
    (
        EQR_PM_RecreateWnd
    );

    {**
    * Called when a property should notify an important change to his owner
    *@param pSender - event sender
    *@param message - message to send to owner
    *}
    TQRNotifyPropOwnerEvent = function(pSender: TObject; message: EQRPropMessages): Boolean of object;

    {**
    * Basic property that can be used on the VCL designer
    *}
    TQRVCLBasicModelComponentPropertyGL = class(TPersistent)
        protected
            m_pOwner:             TControl;
            m_fOnNotifyPropOwner: TQRNotifyPropOwnerEvent;

            {**
            * Invalidate owner, in design time only
            *}
            procedure InvalidateDesigner; virtual;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; virtual;

            {**
            * Constructor
            *@param pOwner - color property owner
            *@param fOnNotifyPropOwner - callback function the prop will use to notify his owner,
            *                            nil if not used
            *}
            constructor Create(pOwner: TControl;
                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent = nil); overload; virtual;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Copies the property attributes from another property
            *@param pSource - source property to copy from
            *}
            procedure Assign(pSource: TPersistent); override;

            { Public properties }
            property Owner:             TControl                read m_pOwner;
            property OnNotifyPropOwner: TQRNotifyPropOwnerEvent read m_fOnNotifyPropOwner write m_fOnNotifyPropOwner;
    end;

    {**
    * Property that exposes a 3D vector on the VCL designer
    *}
    TQRVCLModelComponentVector3GL = class(TQRVCLBasicModelComponentPropertyGL)
        protected
            m_X: Single;
            m_Y: Single;
            m_Z: Single;

            {**
            * Sets the 3D vector X component
            *@param value - 3D vector X component
            *}
            procedure SetX(value: Single); virtual;

            {**
            * Sets the 3D vector Y component
            *@param value - 3D vector Y component
            *}
            procedure SetY(value: Single); virtual;

            {**
            * Sets the 3D vector Z component
            *@param value - 3D vector Z component
            *}
            procedure SetZ(value: Single); virtual;

        public
            {**
            * Constructor
            *}
            constructor Create; override;

            {**
            * Constructor
            *@param pOwner - color property owner
            *@param fOnNotifyPropOwner - callback function the prop will use to notify his owner,
            *                            nil if not used
            *}
            constructor Create(pOwner: TControl;
                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent = nil); override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Copies the property attributes from another property
            *@param pSource - source property to copy from
            *}
            procedure Assign(pSource: TPersistent); override;

        published
            property X: Single read m_X write SetX;
            property Y: Single read m_Y write SetY;
            property Z: Single read m_Z write SetZ;
    end;

    {**
    * Property that exposes a 3D vector on the VCL designer
    *@note Changing a value will recreate the window and reload the model
    *}
    TQRVCLModelComponentNotifiableVector3GL = class(TQRVCLModelComponentVector3GL)
        protected
            {**
            * Sets the 3D vector X component
            *@param value - 3D vector X component
            *}
            procedure SetX(value: Single); override;

            {**
            * Sets the 3D vector Y component
            *@param value - 3D vector Y component
            *}
            procedure SetY(value: Single); override;

            {**
            * Sets the 3D vector Z component
            *@param value - 3D vector Z component
            *}
            procedure SetZ(value: Single); override;

        public
            {**
            * Constructor
            *}
            constructor Create; override;

            {**
            * Constructor
            *@param pOwner - color property owner
            *@param fOnNotifyPropOwner - callback function the prop will use to notify his owner,
            *                            nil if not used
            *}
            constructor Create(pOwner: TControl;
                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent = nil); override;

            {**
            * Destructor
            *}
            destructor Destroy; override;
    end;

    {**
    * Property that exposes a color on the VCL designer
    *}
    TQRVCLModelComponentColorGL = class(TQRVCLBasicModelComponentPropertyGL)
        protected
            m_pColor: TQRVCLColor;

            {**
            * Gets the color red component as single value (between 0.0 and 1.0)
            *@return color red component as single value
            *}
            function GetRedF: Single; virtual;

            {**
            * Gets the color green component as single value (between 0.0 and 1.0)
            *@return color green component as single value
            *}
            function GetGreenF: Single; virtual;

            {**
            * Gets the color blue component as single value (between 0.0 and 1.0)
            *@return color blue component as single value
            *}
            function GetBlueF: Single; virtual;

            {**
            * Gets the color alpha component as single value (between 0.0 and 1.0)
            *@return color alpha component as single value
            *}
            function GetAlphaF: Single; virtual;

            {**
            * Gets the color red component value
            *@return color red component value
            *}
            function GetRed: Byte; virtual;

            {**
            * Sets the color red component value
            *@param value - value to set
            *}
            procedure SetRed(value: Byte); virtual;

            {**
            * Gets the color green component value
            *@return color green component value
            *}
            function GetGreen: Byte; virtual;

            {**
            * Sets the color green component value
            *@param value - value to set
            *}
            procedure SetGreen(value: Byte); virtual;

            {**
            * Gets the color blue component value
            *@return color blue component value
            *}
            function GetBlue: Byte; virtual;

            {**
            * Sets the color blue component value
            *@param value - value to set
            *}
            procedure SetBlue(value: Byte); virtual;

            {**
            * Gets the color alpha component value
            *@return color alpha component value
            *}
            function GetAlpha: Byte; virtual;

            {**
            * Sets the color alpha component value
            *@param value - value to set
            *}
            procedure SetAlpha(value: Byte); virtual;

            {**
            * Gets the used VCL color
            *@return the used VCL color
            *}
            function GetVCLColor: TColor; virtual;

            {**
            * Sets the VCL color to use
            *@param value - VCL color to use
            *}
            procedure SetVCLColor(value: TColor); virtual;

            {**
            * Gets the used Windows color value
            *@return the used Windows color value
            *}
            function GetWinColor: COLORREF; virtual;

            {**
            * Sets the Windows color value to use
            *@param value - Windows color value to use
            *}
            procedure SetWinColor(value: COLORREF); virtual;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; override;

            {**
            * Constructor
            *@param pOwner - color property owner
            *@param fOnNotifyPropOwner - callback function the prop will use to notify his owner,
            *                            nil if not used
            *}
            constructor Create(pOwner: TControl;
                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent = nil); overload; override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Copies the property attributes from another property
            *@param pSource - source property to copy from
            *}
            procedure Assign(pSource: TPersistent); override;

            { Public properties }
            property RedF:        Single      read GetRedF;
            property GreenF:      Single      read GetGreenF;
            property BlueF:       Single      read GetBlueF;
            property AlphaF:      Single      read GetAlphaF;
            property NativeColor: TQRVCLColor read m_pColor;

        published
            { Published properties }
            property Red:      Byte     read GetRed      write SetRed;
            property Green:    Byte     read GetGreen    write SetGreen;
            property Blue:     Byte     read GetBlue     write SetBlue;
            property Alpha:    Byte     read GetAlpha    write SetAlpha    default 255;
            property VCLColor: TColor   read GetVCLColor write SetVCLColor default clBlack;
            property WinColor: COLORREF read GetWinColor write SetWinColor;
    end;

    {**
    * Property that exposes a model color on the VCL designer, black by default
    *@note Changing a value will recreate the window and reload the model
    *}
    TQRVCLModelComponentNotifiableBlackColorGL = class(TQRVCLModelComponentColorGL)
        protected
            {**
            * Sets the color red component value
            *@param value - value to set
            *}
            procedure SetRed(value: Byte); override;

            {**
            * Sets the color green component value
            *@param value - value to set
            *}
            procedure SetGreen(value: Byte); override;

            {**
            * Sets the color blue component value
            *@param value - value to set
            *}
            procedure SetBlue(value: Byte); override;

            {**
            * Sets the color alpha component value
            *@param value - value to set
            *}
            procedure SetAlpha(value: Byte); override;

            {**
            * Sets the VCL color to use
            *@param value - VCL color to use
            *}
            procedure SetVCLColor(value: TColor); override;

            {**
            * Sets the Windows color value to use
            *@param value - Windows color value to use
            *}
            procedure SetWinColor(value: COLORREF); override;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; override;

            {**
            * Constructor
            *@param pOwner - color property owner
            *@param fOnNotifyPropOwner - callback function the prop will use to notify his owner,
            *                            nil if not used
            *}
            constructor Create(pOwner: TControl;
                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent = nil); overload; override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

        published
            property Red      default 0;
            property Green    default 0;
            property Blue     default 0;
            property VCLColor default clBlack;
            property WinColor default $00000000;
    end;

    {**
    * Property that exposes a model color on the VCL designer, white by default
    *@note Changing a value will recreate the window and reload the model
    *}
    TQRVCLModelComponentNotifiableWhiteColorGL = class(TQRVCLModelComponentColorGL)
        protected
            {**
            * Sets the color red component value
            *@param value - value to set
            *}
            procedure SetRed(value: Byte); override;

            {**
            * Sets the color green component value
            *@param value - value to set
            *}
            procedure SetGreen(value: Byte); override;

            {**
            * Sets the color blue component value
            *@param value - value to set
            *}
            procedure SetBlue(value: Byte); override;

            {**
            * Sets the color alpha component value
            *@param value - value to set
            *}
            procedure SetAlpha(value: Byte); override;

            {**
            * Sets the VCL color to use
            *@param value - VCL color to use
            *}
            procedure SetVCLColor(value: TColor); override;

            {**
            * Sets the Windows color value to use
            *@param value - Windows color value to use
            *}
            procedure SetWinColor(value: COLORREF); override;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; override;

            {**
            * Constructor
            *@param pOwner - color property owner
            *@param fOnNotifyPropOwner - callback function the prop will use to notify his owner,
            *                            nil if not used
            *}
            constructor Create(pOwner: TControl;
                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent = nil); overload; override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

        published
            property Red      default 255;
            property Green    default 255;
            property Blue     default 255;
            property VCLColor default clWhite;
            property WinColor default $00FFFFFF;
    end;

    {**
    * Property that exposes the alpha blending controller on the VCL designer
    *}
    TQRVCLModelComponentAlphaBlendingPropertyGL = class(TQRVCLBasicModelComponentPropertyGL)
        protected
            m_GlobalLevel: Byte;
            m_Enabled:     Boolean;

            {**
            * Gets global alpha level
            *@return global alpha level
            *}
            function GetGlobalLevel: Byte; virtual;

            {**
            * Sets global alpha level
            *@param value - value to set
            *}
            procedure SetGlobalLevel(value: Byte); virtual;

            {**
            * Gets enabled flag
            *@return enabled flag
            *}
            function GetEnabled: Boolean; virtual;

            {**
            * Sets enabled flag
            *@param value - if true, alpha blending is enabled
            *}
            procedure SetEnabled(value: Boolean); virtual;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; override;

            {**
            * Constructor
            *@param pOwner - color property owner
            *@param fOnNotifyPropOwner - callback function the prop will use to notify his owner,
            *                            nil if not used
            *}
            constructor Create(pOwner: TControl;
                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent = nil); overload; override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Copies the property attributes from another property
            *@param pSource - source property to copy from
            *}
            procedure Assign(pSource: TPersistent); override;

        published
            { Published properties }
            property GlobalLevel: Byte    read GetGlobalLevel write SetGlobalLevel default 255;
            property Enabled:     Boolean read GetEnabled     write SetEnabled;
    end;

    {**
    * Property that exposes a model on the VCL designer
    *}
    TQRVCLModelComponentPropertyGL = class(TQRVCLBasicModelComponentPropertyGL)
        protected
            m_pColor:          TQRVCLModelComponentNotifiableWhiteColorGL;
            m_pScaling:        TQRVCLModelComponentVector3GL;
            m_pTranslation:    TQRVCLModelComponentVector3GL;
            m_RotationX:       Single;
            m_RotationY:       Single;
            m_RotationZ:       Single;
            m_SwapYZ:          Boolean;
            m_CombinationType: EQRModelMatrixCombinationType;

            {**
            * Sets rotation on X axis
            *@param angle - rotation angle in radians
            *}
            procedure SetRotationX(const angle: Single); virtual;

            {**
            * Sets rotation on Y axis
            *@param angle - rotation angle in radians
            *}
            procedure SetRotationY(const angle: Single); virtual;

            {**
            * Sets rotation on Z axis
            *@param angle - rotation angle in radians
            *}
            procedure SetRotationZ(const angle: Single); virtual;

            {**
            * Sets swap Y and Z axis
            *@param value - if true, Y and Z axiz will be swapped
            *}
            procedure SetSwapYZ(value: Boolean); virtual;

            {**
            * Sets file name
            *@param value - combination type to set
            *}
            procedure SetCombinationType(value: EQRModelMatrixCombinationType); virtual;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; override;

            {**
            * Constructor
            *@param pOwner - color property owner
            *@param fOnNotifyPropOwner - callback function the prop will use to notify his owner,
            *                            nil if not used
            *}
            constructor Create(pOwner: TControl;
                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent = nil); overload; override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Applies properties to model
            *@param pModel - model to update
            *}
            procedure Apply(pModel: TQRModelGroup); virtual;

            {**
            * Copies the property attributes from another property
            *@param pSource - source property to copy from
            *}
            procedure Assign(pSource: TPersistent); override;

        published
            { Published properties }
            property Color:                 TQRVCLModelComponentNotifiableWhiteColorGL read m_pColor          write m_pColor;
            property Scaling:               TQRVCLModelComponentVector3GL              read m_pScaling        write m_pScaling;
            property Translation:           TQRVCLModelComponentVector3GL              read m_pTranslation    write m_pTranslation;
            property RotationX:             Single                                     read m_RotationX       write SetRotationX;
            property RotationY:             Single                                     read m_RotationY       write SetRotationY;
            property RotationZ:             Single                                     read m_RotationZ       write SetRotationZ;
            property SwapYZ:                Boolean                                    read m_SwapYZ          write SetSwapYZ;
            property MatrixCombinationType: EQRModelMatrixCombinationType              read m_CombinationType write SetCombinationType default EQR_CT_Scale_Rotate_Translate;
    end;

    {**
    * Property that exposes a pre-calculated lighting on the VCL designer
    *}
    TQRVCLPreCalculatedLightComponentPropertyGL = class(TQRVCLBasicModelComponentPropertyGL)
        protected
            m_pAmbient:   TQRVCLModelComponentNotifiableBlackColorGL;
            m_pColor:     TQRVCLModelComponentNotifiableWhiteColorGL;
            m_pDirection: TQRVCLModelComponentNotifiableVector3GL;
            m_Enabled:    Boolean;

            {**
            * Sets enabled flag
            *@param value - if true, light is enabled
            }
            procedure SetEnabled(value: Boolean); virtual;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; override;

            {**
            * Constructor
            *@param pOwner - color property owner
            *@param fOnNotifyPropOwner - callback function the prop will use to notify his owner,
            *                            nil if not used
            *}
            constructor Create(pOwner: TControl;
                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent = nil); overload; override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Applies properties to pre-calculated light
            *@param pLight - light to update
            *}
            procedure Apply(pLight: TQRMD2Light); virtual;

            {**
            * Copies the property attributes from another property
            *@param pSource - source property to copy from
            *}
            procedure Assign(pSource: TPersistent); override;

        published
            { Published properties }
            property Ambient:   TQRVCLModelComponentNotifiableBlackColorGL read m_pAmbient   write m_pAmbient;
            property Color:     TQRVCLModelComponentNotifiableWhiteColorGL read m_pColor     write m_pColor;
            property Direction: TQRVCLModelComponentNotifiableVector3GL    read m_pDirection write m_pDirection;
            property Enabled:   Boolean                                    read m_Enabled    write SetEnabled;
    end;

implementation
//------------------------------------------------------------------------------
// TQRVCLBasicModelComponentPropertyGL
//------------------------------------------------------------------------------
constructor TQRVCLBasicModelComponentPropertyGL.Create;
begin
    inherited Create;

    // initialize local variables
    m_pOwner             := nil;
    m_fOnNotifyPropOwner := nil;
end;
//------------------------------------------------------------------------------
constructor TQRVCLBasicModelComponentPropertyGL.Create(pOwner: TControl;
                                           fOnNotifyPropOwner: TQRNotifyPropOwnerEvent);
begin
    inherited Create;

    // initialize local variables
    m_pOwner             := pOwner;
    m_fOnNotifyPropOwner := fOnNotifyPropOwner;
end;
//------------------------------------------------------------------------------
destructor TQRVCLBasicModelComponentPropertyGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLBasicModelComponentPropertyGL.InvalidateDesigner;
begin
    if (not (csDesigning in m_pOwner.ComponentState)) then
        Exit;

    if (not Assigned(m_pOwner)) then
        Exit;

    m_pOwner.Invalidate();
end;
//------------------------------------------------------------------------------
procedure TQRVCLBasicModelComponentPropertyGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLBasicModelComponentPropertyGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLBasicModelComponentPropertyGL)) then
    begin
        // reset values to default
        m_pOwner             := nil;
        m_fOnNotifyPropOwner := nil;
        Exit;
    end;

    // copy content from source
    pSrc                 := pSource as TQRVCLBasicModelComponentPropertyGL;
    m_pOwner             := pSrc.m_pOwner;
    m_fOnNotifyPropOwner := pSrc.m_fOnNotifyPropOwner;
end;
//------------------------------------------------------------------------------
// TQRVCLModelComponentVector3GL
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentVector3GL.Create;
begin
    inherited Create;

    // create local variables
    m_X := 0.0;
    m_Y := 0.0;
    m_Z := 0.0;
end;
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentVector3GL.Create(pOwner: TControl;
                                      fOnNotifyPropOwner: TQRNotifyPropOwnerEvent);
begin
    inherited Create(pOwner, fOnNotifyPropOwner);

    // create local variables
    m_X := 0.0;
    m_Y := 0.0;
    m_Z := 0.0;
end;
//------------------------------------------------------------------------------
destructor TQRVCLModelComponentVector3GL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentVector3GL.SetX(value: Single);
begin
    // no changes?
    if (m_X = value) then
        Exit;

    m_X := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentVector3GL.SetY(value: Single);
begin
    // no changes?
    if (m_Y = value) then
        Exit;

    m_Y := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentVector3GL.SetZ(value: Single);
begin
    // no changes?
    if (m_Z = value) then
        Exit;

    m_Z := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentVector3GL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLModelComponentVector3GL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLModelComponentVector3GL)) then
    begin
        // reset values to default
        m_X := 0.0;
        m_Y := 0.0;
        m_Z := 0.0;
        Exit;
    end;

    // copy content from source
    pSrc := pSource as TQRVCLModelComponentVector3GL;
    m_X  := pSrc.m_X;
    m_Y  := pSrc.m_Y;
    m_Z  := pSrc.m_Z;
end;
//------------------------------------------------------------------------------
// TQRVCLModelComponentNotifiableVector3GL
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentNotifiableVector3GL.Create;
begin
    inherited Create;
end;
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentNotifiableVector3GL.Create(pOwner: TControl;
                                               fOnNotifyPropOwner: TQRNotifyPropOwnerEvent);
begin
    inherited Create(pOwner, fOnNotifyPropOwner);
end;
//------------------------------------------------------------------------------
destructor TQRVCLModelComponentNotifiableVector3GL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableVector3GL.SetX(value: Single);
begin
    // no changes?
    if (m_X = value) then
        Exit;

    inherited SetX(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableVector3GL.SetY(value: Single);
begin
    // no changes?
    if (m_Y = value) then
        Exit;

    inherited SetY(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableVector3GL.SetZ(value: Single);
begin
    // no changes?
    if (m_Z = value) then
        Exit;

    inherited SetZ(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
// TQRVCLModelComponentColorGL
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentColorGL.Create;
begin
    inherited Create;

    // create local variables
    m_pColor := TQRVCLColor.Create;
end;
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentColorGL.Create(pOwner: TControl;
                                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent);
begin
    inherited Create(pOwner, fOnNotifyPropOwner);

    // create local variables
    m_pColor := TQRVCLColor.Create;
end;
//------------------------------------------------------------------------------
destructor TQRVCLModelComponentColorGL.Destroy;
begin
    // clear memory
    m_pColor.Free;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetRedF: Single;
begin
    Result := m_pColor.GetRedF;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetGreenF: Single;
begin
    Result := m_pColor.GetGreenF;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetBlueF: Single;
begin
    Result := m_pColor.GetBlueF;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetAlphaF: Single;
begin
    Result := m_pColor.GetAlphaF;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetRed: Byte;
begin
    Result := m_pColor.R;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentColorGL.SetRed(value: Byte);
begin
    // no changes?
    if (m_pColor.R = value) then
        Exit;

    m_pColor.R := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetGreen: Byte;
begin
    Result := m_pColor.G;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentColorGL.SetGreen(value: Byte);
begin
    // no changes?
    if (m_pColor.G = value) then
        Exit;

    m_pColor.G := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetBlue: Byte;
begin
    Result := m_pColor.B;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentColorGL.SetBlue(value: Byte);
begin
    // no changes?
    if (m_pColor.B = value) then
        Exit;

    m_pColor.B := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetAlpha: Byte;
begin
    Result := m_pColor.A;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentColorGL.SetAlpha(value: Byte);
begin
    // no changes?
    if (m_pColor.A = value) then
        Exit;

    m_pColor.A := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetVCLColor: TColor;
begin
    Result := m_pColor.GetVCLColor;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentColorGL.SetVCLColor(value: TColor);
begin
    // no changes?
    if (m_pColor.GetVCLColor = value) then
        Exit;

    m_pColor.SetVCLColor(value, m_pColor.A);

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentColorGL.GetWinColor: COLORREF;
begin
    Result := m_pColor.GetWinColor;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentColorGL.SetWinColor(value: COLORREF);
begin
    // no changes?
    if (m_pColor.GetWinColor = value) then
        Exit;

    m_pColor.SetWinColor(value, m_pColor.A);

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentColorGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLModelComponentColorGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLModelComponentColorGL)) then
    begin
        // reset values to default
        m_pColor.Assign(nil);
        Exit;
    end;

    // copy content from source
    pSrc := pSource as TQRVCLModelComponentColorGL;
    m_pColor.Assign(pSrc.m_pColor);
end;
//------------------------------------------------------------------------------
// TQRVCLModelComponentNotifiableBlackColorGL
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentNotifiableBlackColorGL.Create;
begin
    inherited Create;

    // by default, set the model color to Black opaque (thus eventual textures will be shown)
    m_pColor.SetVCLColor(clBlack, 255);
end;
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentNotifiableBlackColorGL.Create(pOwner: TControl;
                                                  fOnNotifyPropOwner: TQRNotifyPropOwnerEvent);
begin
    inherited Create(pOwner, fOnNotifyPropOwner);

    // by default, set the model color to Black opaque (thus eventual textures will be shown)
    m_pColor.SetVCLColor(clBlack, 255);
end;
//------------------------------------------------------------------------------
destructor TQRVCLModelComponentNotifiableBlackColorGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableBlackColorGL.SetRed(value: Byte);
begin
    // no changes?
    if (m_pColor.R = value) then
        Exit;

    inherited SetRed(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableBlackColorGL.SetGreen(value: Byte);
begin
    // no changes?
    if (m_pColor.G = value) then
        Exit;

    inherited SetGreen(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableBlackColorGL.SetBlue(value: Byte);
begin
    // no changes?
    if (m_pColor.B = value) then
        Exit;

    inherited SetBlue(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableBlackColorGL.SetAlpha(value: Byte);
begin
    // no changes?
    if (m_pColor.A = value) then
        Exit;

    inherited SetAlpha(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableBlackColorGL.SetVCLColor(value: TColor);
begin
    // no changes?
    if (m_pColor.GetVCLColor = value) then
        Exit;

    inherited SetVCLColor(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableBlackColorGL.SetWinColor(value: COLORREF);
begin
    // no changes?
    if (m_pColor.GetWinColor = value) then
        Exit;

    inherited SetWinColor(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
// TQRVCLModelComponentNotifiableWhiteColorGL
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentNotifiableWhiteColorGL.Create;
begin
    inherited Create;

    // by default, set the model color to white opaque (thus eventual textures will be shown)
    m_pColor.SetVCLColor(clWhite, 255);
end;
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentNotifiableWhiteColorGL.Create(pOwner: TControl;
                                                  fOnNotifyPropOwner: TQRNotifyPropOwnerEvent);
begin
    inherited Create(pOwner, fOnNotifyPropOwner);

    // by default, set the model color to white opaque (thus eventual textures will be shown)
    m_pColor.SetVCLColor(clWhite, 255);
end;
//------------------------------------------------------------------------------
destructor TQRVCLModelComponentNotifiableWhiteColorGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableWhiteColorGL.SetRed(value: Byte);
begin
    // no changes?
    if (m_pColor.R = value) then
        Exit;

    inherited SetRed(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableWhiteColorGL.SetGreen(value: Byte);
begin
    // no changes?
    if (m_pColor.G = value) then
        Exit;

    inherited SetGreen(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableWhiteColorGL.SetBlue(value: Byte);
begin
    // no changes?
    if (m_pColor.B = value) then
        Exit;

    inherited SetBlue(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableWhiteColorGL.SetAlpha(value: Byte);
begin
    // no changes?
    if (m_pColor.A = value) then
        Exit;

    inherited SetAlpha(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableWhiteColorGL.SetVCLColor(value: TColor);
begin
    // no changes?
    if (m_pColor.GetVCLColor = value) then
        Exit;

    inherited SetVCLColor(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentNotifiableWhiteColorGL.SetWinColor(value: COLORREF);
begin
    // no changes?
    if (m_pColor.GetWinColor = value) then
        Exit;

    inherited SetWinColor(value);

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
// TQRVCLModelComponentAlphaBlendingPropertyGL
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentAlphaBlendingPropertyGL.Create;
begin
    inherited Create;

    // initialize local variables
    m_GlobalLevel := 255;
    m_Enabled     := False;
end;
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentAlphaBlendingPropertyGL.Create(pOwner: TControl;
                                                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent);
begin
    inherited Create(pOwner, fOnNotifyPropOwner);

    // initialize local variables
    m_GlobalLevel := 255;
    m_Enabled     := False;
end;
//------------------------------------------------------------------------------
destructor TQRVCLModelComponentAlphaBlendingPropertyGL.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentAlphaBlendingPropertyGL.GetGlobalLevel: Byte;
begin
    Result := m_GlobalLevel;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentAlphaBlendingPropertyGL.SetGlobalLevel(value: Byte);
begin
    // no changes?
    if (m_GlobalLevel = value) then
        Exit;

    m_GlobalLevel := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
function TQRVCLModelComponentAlphaBlendingPropertyGL.GetEnabled: Boolean;
begin
    Result := m_Enabled;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentAlphaBlendingPropertyGL.SetEnabled(value: Boolean);
begin
    // no changes?
    if (m_Enabled = value) then
        Exit;

    m_Enabled := value;

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentAlphaBlendingPropertyGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLModelComponentAlphaBlendingPropertyGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLModelComponentAlphaBlendingPropertyGL)) then
    begin
        // reset values to default
        m_GlobalLevel := 255;
        m_Enabled     := False;
        Exit;
    end;

    // copy content from source
    pSrc          := pSource as TQRVCLModelComponentAlphaBlendingPropertyGL;
    m_GlobalLevel := pSrc.m_GlobalLevel;
    m_Enabled     := pSrc.m_Enabled;
end;
//------------------------------------------------------------------------------
// TQRVCLModelComponentPropertyGL
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentPropertyGL.Create;
begin
    inherited Create;

    // initialize local variables
    m_pColor          := TQRVCLModelComponentNotifiableWhiteColorGL.Create;
    m_pScaling        := TQRVCLModelComponentVector3GL.Create;
    m_pTranslation    := TQRVCLModelComponentVector3GL.Create;
    m_RotationX       := 0.0;
    m_RotationY       := 0.0;
    m_RotationZ       := 0.0;
    m_SwapYZ          := False;
    m_CombinationType := EQR_CT_Scale_Rotate_Translate;

    m_pScaling.m_X     := 1.0;
    m_pScaling.m_Y     := 1.0;
    m_pScaling.m_Z     := 1.0;
    m_pTranslation.m_X := 0.0;
    m_pTranslation.m_Y := 0.0;
    m_pTranslation.m_Z := 0.0;
end;
//------------------------------------------------------------------------------
constructor TQRVCLModelComponentPropertyGL.Create(pOwner: TControl;
                                      fOnNotifyPropOwner: TQRNotifyPropOwnerEvent);
begin
    inherited Create(pOwner, fOnNotifyPropOwner);

    // initialize local variables
    m_pColor           := TQRVCLModelComponentNotifiableWhiteColorGL.Create(pOwner, fOnNotifyPropOwner);
    m_pScaling         := TQRVCLModelComponentVector3GL.Create(pOwner, fOnNotifyPropOwner);
    m_pTranslation     := TQRVCLModelComponentVector3GL.Create(pOwner, fOnNotifyPropOwner);
    m_RotationX        := 0.0;
    m_RotationY        := 0.0;
    m_RotationZ        := 0.0;
    m_pScaling.m_X     := 1.0;
    m_pScaling.m_Y     := 1.0;
    m_pScaling.m_Z     := 1.0;
    m_pTranslation.m_X := 0.0;
    m_pTranslation.m_Y := 0.0;
    m_pTranslation.m_Z := 0.0;
    m_SwapYZ           := False;
    m_CombinationType  := EQR_CT_Scale_Rotate_Translate;
end;
//------------------------------------------------------------------------------
destructor TQRVCLModelComponentPropertyGL.Destroy;
begin
    // clear memory
    m_pColor.Free;
    m_pScaling.Free;
    m_pTranslation.Free;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentPropertyGL.SetRotationX(const angle: Single);
begin
    // no changes?
    if (m_RotationX = angle) then
        Exit;

    m_RotationX := angle;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentPropertyGL.SetRotationY(const angle: Single);
begin
    // no changes?
    if (m_RotationY = angle) then
        Exit;

    m_RotationY := angle;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentPropertyGL.SetRotationZ(const angle: Single);
begin
    // no changes?
    if (m_RotationZ = angle) then
        Exit;

    m_RotationZ := angle;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentPropertyGL.SetSwapYZ(value: Boolean);
begin
    // no changes?
    if (m_SwapYZ = value) then
        Exit;

    m_SwapYZ := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentPropertyGL.SetCombinationType(value: EQRModelMatrixCombinationType);
begin
    // no changes?
    if (m_CombinationType = value) then
        Exit;

    m_CombinationType := value;

    InvalidateDesigner;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentPropertyGL.Apply(pModel: TQRModelGroup);
begin
    // no model?
    if (not Assigned(pModel)) then
        Exit;

    // model translation changed?
    if ((m_pTranslation.m_X <> pModel.Translation.X) or
        (m_pTranslation.m_Y <> pModel.Translation.Y) or
        (m_pTranslation.m_Z <> pModel.Translation.Z))
    then
        // update model translation
        pModel.Translation^ := TQRVector3D.Create(m_pTranslation.m_X,
                                                  m_pTranslation.m_Y,
                                                  m_pTranslation.m_Z);

    // model rotation changed on X axis?
    if (m_RotationX <> pModel.RotationX) then
        // update model rotation on X axis
        pModel.RotationX := m_RotationX;

    // model rotation changed on Y axis?
    if (m_RotationY <> pModel.RotationY) then
        // update model rotation on Y axis
        pModel.RotationY := m_RotationY;

    // model rotation changed on Z axis?
    if (m_RotationZ <> pModel.RotationZ) then
        // update model rotation on X axis
        pModel.RotationZ := m_RotationZ;

    // model scaling changed?
    if ((m_pScaling.m_X <> pModel.Scaling.X) or
        (m_pScaling.m_Y <> pModel.Scaling.Y) or
        (m_pScaling.m_Z <> pModel.Scaling.Z))
    then
        // update model scaling
        pModel.Scaling^ := TQRVector3D.Create(m_pScaling.m_X, m_pScaling.m_Y, m_pScaling.m_Z);

    // swapping y/Z changed?
    if (m_SwapYZ <> pModel.SwapYZ) then
        // update model swapping
        pModel.SwapYZ := m_SwapYZ;

        // model matrix combination type changed?
    if (m_CombinationType <> pModel.MatrixCombinationType) then
        // update model matrix combination type
        pModel.MatrixCombinationType := m_CombinationType;
end;
//------------------------------------------------------------------------------
procedure TQRVCLModelComponentPropertyGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLModelComponentPropertyGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLModelComponentPropertyGL)) then
    begin
        // reset values to default
        m_RotationX       := 0.0;
        m_RotationY       := 0.0;
        m_RotationZ       := 0.0;
        m_SwapYZ          := False;
        m_CombinationType := EQR_CT_Scale_Rotate_Translate;

        m_pColor.Assign(nil);
        m_pScaling.Assign(nil);
        m_pTranslation.Assign(nil);
        Exit;
    end;

    // copy content from source
    pSrc              := pSource as TQRVCLModelComponentPropertyGL;
    m_RotationX       := pSrc.m_RotationX;
    m_RotationY       := pSrc.m_RotationY;
    m_RotationZ       := pSrc.m_RotationZ;
    m_SwapYZ          := pSrc.m_SwapYZ;
    m_CombinationType := pSrc.m_CombinationType;

    m_pColor.Assign(pSrc.m_pColor);
    m_pScaling.Assign(pSrc.m_pScaling);
    m_pTranslation.Assign(pSrc.m_pTranslation);
end;
//------------------------------------------------------------------------------
// TQRVCLPreCalculatedLightComponentPropertyGL
//------------------------------------------------------------------------------
constructor TQRVCLPreCalculatedLightComponentPropertyGL.Create;
begin
    inherited Create;

    // initialize local variables
    m_pAmbient   := TQRVCLModelComponentNotifiableBlackColorGL.Create;
    m_pColor     := TQRVCLModelComponentNotifiableWhiteColorGL.Create;
    m_pDirection := TQRVCLModelComponentNotifiableVector3GL.Create;
    m_Enabled    := False;
end;
//------------------------------------------------------------------------------
constructor TQRVCLPreCalculatedLightComponentPropertyGL.Create(pOwner: TControl;
                                                   fOnNotifyPropOwner: TQRNotifyPropOwnerEvent);
begin
    inherited Create(pOwner, fOnNotifyPropOwner);

    // initialize local variables
    m_pAmbient   := TQRVCLModelComponentNotifiableBlackColorGL.Create(pOwner, fOnNotifyPropOwner);
    m_pColor     := TQRVCLModelComponentNotifiableWhiteColorGL.Create(pOwner, fOnNotifyPropOwner);
    m_pDirection := TQRVCLModelComponentNotifiableVector3GL.Create(pOwner, fOnNotifyPropOwner);
    m_Enabled    := False;
end;
//------------------------------------------------------------------------------
destructor TQRVCLPreCalculatedLightComponentPropertyGL.Destroy;
begin
    inherited Destroy;

    // clear memory
    m_pDirection.Free;
    m_pColor.Free;
    m_pAmbient.Free;
end;
//------------------------------------------------------------------------------
procedure TQRVCLPreCalculatedLightComponentPropertyGL.SetEnabled(value: Boolean);
begin
    // no changes?
    if (m_Enabled = value) then
        Exit;

    m_Enabled := value;

    // notify parent that windows should be recreated
    if (Assigned(m_fOnNotifyPropOwner)) then
        m_fOnNotifyPropOwner(Self, EQR_PM_RecreateWnd);
end;
//------------------------------------------------------------------------------
procedure TQRVCLPreCalculatedLightComponentPropertyGL.Apply(pLight: TQRMD2Light);
begin
    // no light?
    if (not Assigned(pLight)) then
        Exit;

    // is light disabled?
    if (not m_Enabled) then
    begin
        pLight.Enabled := False;
        Exit;
    end;

    // copy light values and enable light
    pLight.Ambient.R   := m_pAmbient.Red;
    pLight.Ambient.G   := m_pAmbient.Green;
    pLight.Ambient.B   := m_pAmbient.Blue;
    pLight.Ambient.A   := m_pAmbient.Alpha;
    pLight.Color.R     := m_pColor.Red;
    pLight.Color.G     := m_pColor.Green;
    pLight.Color.B     := m_pColor.Blue;
    pLight.Color.A     := m_pColor.Alpha;
    pLight.Direction.X := m_pDirection.X;
    pLight.Direction.Y := m_pDirection.Y;
    pLight.Direction.Z := m_pDirection.Z;
    pLight.Enabled     := True;
end;
//------------------------------------------------------------------------------
procedure TQRVCLPreCalculatedLightComponentPropertyGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLPreCalculatedLightComponentPropertyGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLPreCalculatedLightComponentPropertyGL)) then
    begin
        // reset values to default
        m_Enabled := False;

        m_pAmbient.Assign(nil);
        m_pColor.Assign(nil);
        m_pDirection.Assign(nil);
        Exit;
    end;

    // copy content from source
    pSrc      := pSource as TQRVCLPreCalculatedLightComponentPropertyGL;
    m_Enabled := pSrc.m_Enabled;

    m_pAmbient.Assign(pSrc.m_pAmbient);
    m_pColor.Assign(pSrc.m_pColor);
    m_pDirection.Assign(pSrc.m_pDirection);
end;
//------------------------------------------------------------------------------

end.
