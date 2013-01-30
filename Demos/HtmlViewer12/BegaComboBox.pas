{-------------------------------------------------------------------------------
$File$, (C) 2006 by Bernd Gabriel. All rights reserved.
--------------------------------------------------------------------------------
Author       : $Author: Bernd $
Last CheckIn : $Date: 2009/08/26 21:51:25 $
Revision     : $Revision: 1.6 $

mailto: info@fast-function-factory.de

Log:
====

$Log: BegaComboBox.pas,v $
Revision 1.6  2009/08/26 21:51:25  Bernd
Insert new items at top of the list.

Revision 1.5  2009/06/28 21:00:24  Bernd
BegaViewer enhancements:
- BegaComboBox
- Thumbnails Settings

Revision 1.4  2007/06/07 20:30:31  Bernd
Delphi 10 compatibility and some minor enhancements

Revision 1.3  2007/03/26 22:50:19  Bernd
Html print preview form

Revision 1.1  2006/02/21 09:42:23  gabriel
added:
- BegaComboBox
- BegaVSTComboBox


TBegaComboBox is a combobox with arbitrary content in drop down control.
Some combobox properties:
  ReadOnly: edit control cannot edit.
            drop down control can drop down, but cannot select/edit.
  Editable: edit control can edit (if combobox is not readonly)
  ComboOptions:
    ceoEditableEvenIfDroppedDown: if drop down control is dropped down, edit
      control remains editable, else it is readonly while dropped down

There are 2 major types of drop down controls:

1) readonly selector
 - vertical cursor moves select items in drop down control.
 - if edit control is not readonly, editing input and horizontal cursor moves go
   to the edit control and may control the offered items in drop down control.
 - default or cancel action closes drop down (VK_RETURN, VK_ESCAPE)
 - clicking inside drop down generates default action.
 - clicking outside drop down generates cancel action.
 - edit may be editable or not

2) readwrite control
 - any input goes to the drop down control until drop down closes.
 - edit control is updated with resulting text/description.
 - default or cancel action closes drop down (VK_RETURN, VK_ESCAPE)
 - clicking outside drop down generates cancel action.
-------------------------------------------------------------------------------}

unit BegaComboBox;

interface

uses
  Windows, Buttons, Classes, Controls, StdCtrls, Forms, Graphics,
  Mask, Math, Menus, Messages, Variants, Registry, SysUtils,
  //
  BegaObjects;

const
  WM_CLOSEDROPDOWN = WM_APP + 39;

type
  TBegaCustomComboboxState = set of (cbsItemsModified);
  TBegaCustomComboboxOptions = set of (cboAutoSaveItems);
const
  CBegaCustomComboboxOptionsDefault = [];

type
  //BG, 18.05.2009
  TBegaCustomCombobox = class(TComboBox)
  private
    FState: TBegaCustomComboboxState;
    FOptions: TBegaCustomComboboxOptions;
    FOnLoadItems: TNotifyEvent;
    FOnSaveItems: TNotifyEvent;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    property OnLoadItems: TNotifyEvent read FOnLoadItems write FOnLoadItems;
    property OnSaveItems: TNotifyEvent read FOnSaveItems write FOnSaveItems;
    property Options: TBegaCustomComboboxOptions read FOptions write FOptions default CBegaCustomComboboxOptionsDefault;
    property ComboboxState: TBegaCustomComboboxState read FState;
  public
    constructor Create(AOwner: TComponent); override;
    function UpdateItem(const Item: string; const AObject: TObject): Integer;
    procedure AddItem(Item: string; AObject: TObject); override;
    procedure ClearItems; 
    procedure BeforeDestruction; override;
    procedure LoadItems; virtual;
    procedure RemoveItem(Index: Integer);
    procedure SaveItems; virtual;
  end;

  // BG, 16.05.2009
  TBegaCombobox = class(TBegaCustomCombobox)
  private
    FRegistryKey: String;
    FRegistryValuePrefix: String;
    function ValidateRegistryKey(const RegistryKey: String): String;
    function ValidateValuePrefix(const ValuePrefix: String): String;
    function ValidateValueName(const ValuePrefix: String; Index: Integer): String;
  protected
    procedure LoadFromRegistry(const RegistryKey, ValuePrefix: String);
    procedure SaveToRegistry(const RegistryKey, ValuePrefix: String);
  public
    procedure AfterConstruction; override;
    procedure LoadItems; override;
    procedure SaveItems; override;
    property ComboboxState;
  published
    property Options;
    property RegistryKey: String read FRegistryKey write FRegistryKey;
    property RegistryValuePrefix: String read FRegistryValuePrefix write FRegistryValuePrefix;
  end;

{ TBegaButton }

  TBegaComboButton = class(TSpeedButton)
  // TButton/TBitBtn get the focus when pushed and thus close drop down control
  // immediatelly. This assumes not to accept the values in drop down control.
  // If using these button types, change buttonClick method in TBegaComboBox.
  end;

{ TBegaCustomDropDownControl }

  TBegaCustomDropDownControl = class(TScrollBox)
  private
  protected
    function getEditText: String; virtual; abstract;
    procedure accept(var Text: String); virtual; abstract;
    procedure backup(var Text: String); virtual; abstract;
    procedure cancel(var Text: String); virtual; abstract;
    procedure CreateParams(var Params: TCreateParams); override;
    property EditText: String read getEditText;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
//    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
//    procedure WMNCActivate(var Message: TWMNCActivate); message WM_NCACTIVATE;
//    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Dispatch(var Message); override;
  end;

{ TBegaCustomComboEdit }
type
  TBegaCustomComboEditOption = (ceoEditableEvenIfDroppedDown);
  TBegaCustomComboEditOptions = set of TBegaCustomComboEditOption;
const
  CDefaultComboEditOptions = [ceoEditableEvenIfDroppedDown];

type
  TBegaCustomComboEdit = class;

  TBegaStringEvent = procedure(Sender: TObject; var Text: String) of object;
  TBegaOnDroppingDown = procedure(Sender: TBegaCustomComboEdit; var CanShow: Boolean) of object;
  TBegaChangedComboOptionsEvent = procedure(Sender: TBegaCustomComboEdit; const ChangedOptions: TBegaCustomComboEditOptions) of object;

  TPopupAlign = (epaLeft, epaRight);
  TGlyphKind = (gkCustom, gkDefault, gkDropDown, gkEllipsis);

  TBegaCustomComboEdit = class(TCustomMaskEdit)
  private
    // components
    FBtnControl: TWinControl; // place button(s) on it.
    FButton: TBegaComboButton;  // the on and only button by now
    FDefNumGlyphs: TNumGlyphs;
    FDropDown: TBegaCustomDropDownControl;
    FGlyphKind: TGlyphKind;
    // events
    FOnDroppingDown: TBegaOnDroppingDown;
    FOnAccept: TBegaStringEvent;
    FOnBackup: TBegaStringEvent;
    FOnCancel: TBegaStringEvent;
    FOnChangedComboOptions: TBegaChangedComboOptionsEvent;
    // properties
    FAlignment: TAlignment;
    FDropDownKey: TShortCut;
    FEditable: Boolean;
    FPopupAlign: TPopupAlign;
    FReadOnly: Boolean;
    // states
    FComboOptions: TBegaCustomComboEditOptions;
    FBackupText: String;
    //
    function BtnWidthStored: Boolean;
    function GetButtonHint: string;
    function GetButtonWidth: Integer;
    function getDroppedDown: Boolean;
    function GetGlyph: TBitmap;
    function GetMinHeight: Integer;
    function GetNumGlyphs: TNumGlyphs;
    function GetTextHeight: Integer;
    function IsCustomGlyph: Boolean;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNCtlColor(var Message: TMessage); message CN_CTLCOLOREDIT;
    procedure EditButtonClick(Sender: TObject);
    procedure RecreateGlyph;
    procedure SetAlignment(Value: TAlignment);
    procedure SetButtonHint(const Value: string);
    procedure SetButtonWidth(Value: Integer);
    procedure SetEditable(Value: Boolean);
    procedure SetEditRect;
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphKind(Value: TGlyphKind);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDropDownControl(Value: TBegaCustomDropDownControl);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateBtnBounds;
    procedure WMClosedDropDown(var Message: TMessage); message WM_CLOSEDROPDOWN;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMChanged(var Message: TCMChanged); message CM_CHANGED;
    //BG, 12.02.2006
    procedure updateEditable;

    // them
    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; virtual;
    procedure AdjustHeight;
    procedure toggleDropDown; dynamic;
    procedure setComboOptions(const Value: TBegaCustomComboEditOptions);
    procedure setDroppedDown(const Value: Boolean);
  protected
    //BG, 11.02.2006
    function droppingDown: Boolean; virtual;
    function EditCanModify: Boolean; override;
    procedure doAccept; virtual;
    procedure doBackup; virtual;
    procedure doCancel; virtual;
    procedure doChangedComboOptions(ChangedOptions: TBegaCustomComboEditOptions); virtual;

    procedure doHideDropDown; virtual;
    procedure doShowDropDown; virtual;
    procedure getDropDownRect(var ALeft, ATop, AWidth, AHeight, AFlags: Integer); virtual;
    procedure Loaded; override;
    property ComboOptions: TBegaCustomComboEditOptions read FComboOptions write setComboOptions;
    property DropDownKey: TShortCut read FDropDownKey write FDropDownKey;
    property OnAccept: TBegaStringEvent read FOnAccept write FOnAccept;
    property OnBackup: TBegaStringEvent read FOnBackup write FOnBackup;
    property OnCancel: TBegaStringEvent read FOnCancel write FOnCancel;
    property OnChangedComboOptions: TBegaChangedComboOptionsEvent read FOnChangedComboOptions write FOnChangedComboOptions;
    property OnDroppingDown: TBegaOnDroppingDown read FOnDroppingDown write FOnDroppingDown;

    // them
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    //
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth stored BtnWidthStored;
    property DroppedDown: Boolean read getDroppedDown write setDroppedDown;
    property Editable: Boolean read FEditable write SetEditable default True;
    property GlyphKind: TGlyphKind read FGlyphKind write SetGlyphKind default gkCustom;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property PopupAlign: TPopupAlign read FPopupAlign write FPopupAlign default epaLeft;
    property DropDownControl: TBegaCustomDropDownControl read FDropDown write setDropDownControl;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure hideDropDown;
    procedure showDropDown; virtual;
    procedure updateData(AcceptFromDropDown: Boolean); virtual;
  end;

{ TBegaComboEdit }

  TBegaComboEdit = class(TBegaCustomComboEdit)
  public
    property DropDownControl;
  published
    property Alignment;
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property CharCase;
    property Color;
    property ComboOptions;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownKey;
    property Editable;
    property EditMask;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is published before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumGlyphs;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    // events
    property OnAccept;
    property OnBackup;
    property OnCancel;
    property OnChange;
    property OnChangedComboOptions;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDroppingDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

const
  DefEditBtnWidth = 21;

function getsFocus(AControl: TWinControl; AHandle: HWND): Boolean;
var
  Index: Integer;
begin
  Result := TRUE;
  if AControl.HandleAllocated and (AControl.Handle = AHandle) then
    exit;
  for Index := 0 to AControl.ControlCount - 1 do
    if AControl.Controls[Index] is TWinControl then
      if getsFocus(TWinControl(AControl.Controls[Index]), AHandle) then
        exit;
  Result := FALSE;
end;

{ TBegaCustomDropDownControl }

//- BG ----------------------------------------------------------- 12.02.2006 --
constructor TBegaCustomDropDownControl.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable, csAcceptsControls];
  Ctl3D := False;
  ParentCtl3D := False;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Visible := False;
end;

//- BG ----------------------------------------------------------- 11.02.2006 --
procedure TBegaCustomDropDownControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := WS_POPUP or {WS_BORDER or} WS_CLIPCHILDREN or WS_SIZEBOX;
    //ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

//- BG ----------------------------------------------------------- 18.02.2006 --
procedure TBegaCustomDropDownControl.Dispatch(var Message);
begin
  inherited;
end;

//- BG ----------------------------------------------------------- 18.02.2006 --
procedure TBegaCustomDropDownControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_ESCAPE:
    begin
      Key := 0;
      hide;
      if Parent.CanFocus then
        Parent.SetFocus;
      PostMessage(Parent.Handle, WM_CLOSEDROPDOWN, 0, 0);
    end;
    VK_RETURN:
    begin
      Key := 0;
      hide;
      if Parent.CanFocus then
        Parent.SetFocus;
      PostMessage(Parent.Handle, WM_CLOSEDROPDOWN, 1, 0);
    end;
  end;
end;

//- BG ----------------------------------------------------------- 16.02.2006 --
procedure TBegaCustomDropDownControl.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if getsFocus(self, Message.FocusedWnd) then
  begin
    if Parent.CanFocus then
      Parent.SetFocus;
  end
  else if getsFocus(Parent, Message.FocusedWnd) then
  begin
    if Parent.CanFocus then
      Parent.SetFocus;
  end
  else
  begin
    Hide;
    if Parent.CanFocus then
      Parent.SetFocus;
  end;
end;

////- BG ----------------------------------------------------------- 17.02.2006 --
//procedure TBegaCustomDropDownControl.WMMouseActivate(
//  var Message: TMessage);
//begin
////  Message.Result := MA_NOACTIVATE;
//  //PostMessage(Parent.Handle, WM_CLOSEDROPDOWN, 0, 0);
//  //Hide;
////  if Parent.CanFocus then
////    Parent.SetFocus;
//end;
//
////- BG ----------------------------------------------------------- 18.02.2006 --
//procedure TBegaCustomDropDownControl.WMNCActivate(var Message: TWMNCActivate);
//begin
////  Message.Result := MA_NOACTIVATE;
////  if Parent.CanFocus then
////    Parent.SetFocus;
//end;
//
////- BG ----------------------------------------------------------- 18.02.2006 --
//procedure TBegaCustomDropDownControl.WMSetFocus(var Message: TWMSetFocus);
//begin
//  Message.Result := 0;
//  if Parent.CanFocus then
//    Parent.SetFocus;
//end;

{ TBegaCustomComboEdit }

//- BG ----------------------------------------------------------- 13.02.2006 --
procedure TBegaCustomComboEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FDropDown) and
    (Message.Sender <> FButton) and ((FDropDown <> nil) and
    not FDropDown.ContainsControl(Message.Sender)) then
    HideDropDown;
end;

//- BG ----------------------------------------------------------- 13.02.2006 --
procedure TBegaCustomComboEdit.CMChanged(var Message: TCMChanged);
begin
  //FDropDown.Changed requests to update the displayed edit text.
  if (Message.Child = FDropDown) and (FDropDown <> nil) then
  begin
    Text := FDropDown.EditText;
    SelectAll;
  end;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
constructor TBegaCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  AutoSize := False;
  FEditable := True;
  FDropDownKey := ShortCut(VK_DOWN, []);
  FPopupAlign := epaLeft;
  FDefNumGlyphs := 1;
  FGlyphKind := gkCustom;
  FComboOptions := CDefaultComboEditOptions;

  FBtnControl := TWinControl.Create(Self);
  with FBtnControl do
    ControlStyle := ControlStyle + [csReplicatable];
  FBtnControl.Width := DefEditBtnWidth;
  FBtnControl.Height := 17;
  FBtnControl.Visible := True;
  FBtnControl.Parent := Self;
  FButton := TBegaComboButton.Create(Self);
  FButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Visible := True;
  FButton.Parent := FBtnControl;
  FButton.OnClick := EditButtonClick;
  Height := 21;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Longword = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN
    or Alignments[FAlignment];
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
destructor TBegaCustomComboEdit.Destroy;
begin
  FButton.OnClick := nil;
  inherited Destroy;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.doAccept;
begin
  FBackupText := FDropDown.EditText;
  FDropDown.accept(FBackupText);
  if assigned(FOnAccept) then
    FOnAccept(self, FBackupText);
  Text := FBackupText;
end;

//- BG ----------------------------------------------------------- 13.02.2006 --
procedure TBegaCustomComboEdit.doBackup;
begin
  FBackupText := Text;
  FDropDown.backup(FBackupText);
  if assigned(FOnBackup) then
    FOnBackup(self, FBackupText);
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.doCancel;
begin
  FDropDown.cancel(FBackupText);
  if assigned(FOnCancel) then
    FOnCancel(self, FBackupText);
  Text := FBackupText;
end;

//- BG ----------------------------------------------------------- 11.02.2006 --
procedure TBegaCustomComboEdit.doHideDropDown;
begin
  FDropDown.Hide;
  if CanFocus then
    SetFocus;
end;

//- BG ----------------------------------------------------------- 11.02.2006 --
procedure TBegaCustomComboEdit.doShowDropDown;
var
  Left, Top, Width, Height, Flags: Integer;
begin
  Left   := FDropDown.Left;
  Top    := FDropDown.Top;
  Width  := FDropDown.Width;
  Height := FDropDown.Height;
  Flags  := SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW;
  getDropDownRect(Left, Top, Width, Height, Flags);
  SetWindowPos(FDropDown.Handle, HWND_TOP, Left, Top, Width, Height, Flags);
  FDropDown.Show;
end;

//- BG ----------------------------------------------------------- 11.02.2006 --
function TBegaCustomComboEdit.DroppingDown: Boolean;
begin
  Result := TRUE;
  if assigned(FOnDroppingDown) then
    FOnDroppingDown(self, Result);
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.EditButtonClick(Sender: TObject);
begin
  ToggleDropDown;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
function TBegaCustomComboEdit.EditCanModify: Boolean;
begin
  if ReadOnly or not Editable then
    Result := False
  else
    if DroppedDown then
      Result := ceoEditableEvenIfDroppedDown in FComboOptions
    else
      Result := True
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.GetDropDownRect(var ALeft, ATop, AWidth, AHeight, AFlags: Integer);
var
  Pt: TPoint;
  //Y: Integer;
begin
  // width
  if AWidth = 0 then
  begin
    AWidth := Width;
    AFlags := AFlags and not SWP_NOSIZE;
  end;
  // height
  if AHeight = 0 then
  begin
    AHeight := ClientHeight * 16;
    AFlags := AFlags and not SWP_NOSIZE;
  end;
  // top
  Pt := Parent.ClientToScreen(Point(Left, Top));
  ATop := Pt.Y + Height;
  if ATop + AHeight > Screen.Height then
    ATop := Pt.Y - AHeight;
  // left
  ALeft := Pt.X;
  case FPopupAlign of
    epaRight:
      begin
        Dec(ALeft, AWidth - Width);
        if ALeft < 0 then
          Inc(ALeft, AWidth - Width);
      end;
    epaLeft:
      begin
        if ALeft + AWidth > Screen.Width then
          Dec(ALeft, AWidth - Width);
      end;
  end;
  if ALeft < 0 then
    ALeft := 0
  else if ALeft + AWidth > Screen.Width then
    ALeft := Screen.Width - AWidth;
  AFlags := AFlags and not SWP_NOMOVE;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
function TBegaCustomComboEdit.getDroppedDown: Boolean;
begin
  Result := (FDropDown <> nil) and FDropDown.Visible;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.hideDropDown;
begin
  if DroppedDown then
  begin
    doHideDropDown;
    updateEditable;
  end;
end;

//- BG ----------------------------------------------------------- 13.02.2006 --
procedure TBegaCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if DroppedDown then
  begin
    FDropDown.KeyDown(Key, Shift);
    if Key = 0 then
    begin
      Text := FDropDown.EditText;
      SelectAll;
    end;
  end
  else if ShortCut(Key, Shift) = FDropDownKey then
    showDropDown;
end;

//- BG ----------------------------------------------------------- 11.02.2006 --
procedure TBegaCustomComboEdit.KeyPress(var Key: Char);

  procedure closePopup(Accept: Boolean);
  begin
    hideDropDown;
    updateData(Accept);
    Key := #0;
  end;

begin
  if DroppedDown then
    case Key of
      Char(VK_RETURN): closePopup(True);
      Char(VK_ESCAPE): closePopup(False);
    end;
  inherited KeyPress(Key);
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.Loaded;
begin
  inherited;
  updateEditable;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

//- BG ----------------------------------------------------------- 11.02.2006 --
procedure TBegaCustomComboEdit.SetDropDownControl(Value: TBegaCustomDropDownControl);
begin
  if FDropDown <> Value then
  begin
    if FDropDown <> nil then
      FDropDown.Parent := nil;
    FDropDown := Value;
    FDropDown.Parent := self;
  end;
end;

//- BG ----------------------------------------------------------- 14.02.2006 --
procedure TBegaCustomComboEdit.setDroppedDown(const Value: Boolean);
begin
  if DroppedDown <> Value then
    if Value then
      showDropDown
    else
      hideDropDown
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.SetEditable(Value: Boolean);
begin
  if FEditable <> Value then
  begin
    FEditable := Value;
    updateEditable;
  end;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    updateEditable;
  end;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.showDropDown;
begin
  if FDropDown <> nil then
    if not DroppedDown then
      if DroppingDown then
      begin
        if CanFocus then
          SetFocus;
        doBackup;
        doShowDropDown;
        updateEditable;
      end;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.toggleDropDown;
begin
  if DroppedDown then
  begin
    updateData(False);
    hideDropDown;
  end
  else
    showDropDown;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.updateData(AcceptFromDropDown: Boolean);
begin
  if AcceptFromDropDown and not ReadOnly then
    doAccept
  else
    doCancel;
  invalidate;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.updateEditable;
begin
  if csLoading in ComponentState then
    exit;
  if EditCanModify = inherited ReadOnly then
  begin
    inherited ReadOnly := not EditCanModify;
    if EditCanModify then
      ShowCaret(Handle)
    else
      HideCaret(Handle)
  end
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.WMCut(var Message: TWMCut);
begin
  if EditCanModify then
    inherited;
end;

//- BG ----------------------------------------------------------- 13.02.2006 --
procedure TBegaCustomComboEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if DroppedDown then
    if not (getsFocus(self, Message.FocusedWnd) or getsFocus(FDropDown, Message.FocusedWnd)) then
      doHideDropDown;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.WMPaste(var Message: TWMPaste);
begin
  if EditCanModify then
    inherited;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.WMClosedDropDown(var Message: TMessage);
begin
  updateEditable;
  updateData(Message.WParam <> 0);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function TBegaCustomComboEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

procedure TBegaCustomComboEdit.SetButtonWidth(Value: Integer);
begin
  if ButtonWidth <> Value then begin
    FBtnControl.Visible := Value > 1;
    if (csCreating in ControlState) then begin
      FBtnControl.Width := Value;
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      RecreateGlyph;
    end
    else if (Value <> ButtonWidth) and (Value < ClientWidth) then begin
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      if HandleAllocated then RecreateWnd;
      RecreateGlyph;
    end;
  end;
end;

function TBegaCustomComboEdit.GetButtonHint: string;
begin
  Result := FButton.Hint;
end;

procedure TBegaCustomComboEdit.SetButtonHint(const Value: string);
begin
  FButton.Hint := Value;
end;

function TBegaCustomComboEdit.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TBegaCustomComboEdit.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
  FGlyphKind := gkCustom;
end;

function TBegaCustomComboEdit.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TBegaCustomComboEdit.SetNumGlyphs(Value: TNumGlyphs);
begin
  if FGlyphKind in [gkDropDown, gkEllipsis] then FButton.NumGlyphs := 1
  else if FGlyphKind = gkDefault then FButton.NumGlyphs := FDefNumGlyphs
  else FButton.NumGlyphs := Value;
end;

procedure TBegaCustomComboEdit.SetEditRect;
var
  Loc: TRect;
begin
  AdjustHeight;
  SetRect(Loc, 0, 0, ClientWidth - FBtnControl.Width - 2, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TBegaCustomComboEdit.UpdateBtnBounds;
var
  BtnRect: TRect;
begin
{$IFDEF WIN32}
  if NewStyleControls then begin
    if Ctl3D and (BorderStyle = bsSingle) then
      BtnRect := Bounds(Width - FButton.Width - 4, 0,
        FButton.Width, Height - 4)
    else begin
      if BorderStyle = bsSingle then
        BtnRect := Bounds(Width - FButton.Width - 2, 2,
          FButton.Width, Height - 4)
      else
        BtnRect := Bounds(Width - FButton.Width, 0,
          FButton.Width, Height);
    end;
  end
  else
    BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
{$ELSE}
  BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
{$ENDIF}
  with BtnRect do
    FBtnControl.SetBounds(Left, Top, Right - Left, Bottom - Top);
  FButton.Height := FBtnControl.Height;
  SetEditRect;
end;

{$IFDEF WIN32}
procedure TBegaCustomComboEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  UpdateBtnBounds;
end;
{$ENDIF}

procedure TBegaCustomComboEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    MinHeight := GetMinHeight;
    { text edit bug: if size less than MinHeight, then edit ctrl does
      not display the text }
    if Height < MinHeight then begin
      Height := MinHeight;
      Exit;
    end;
  end
  else begin
    if (FDropDown <> nil) and (csDesigning in ComponentState) then
      FDropDown.SetBounds(0, Height + 1, 10, 10);
  end;
  UpdateBtnBounds;
end;

procedure TBegaCustomComboEdit.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D then I := 8 else I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Height := Metrics.tmHeight + I;
end;

function TBegaCustomComboEdit.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Metrics.tmHeight;
end;

function TBegaCustomComboEdit.GetMinHeight: Integer;
var
  I: Integer;
begin
  I := GetTextHeight;
  Result := I + GetSystemMetrics(SM_CYBORDER) * 4 +
    1 {$IFNDEF WIN32} + (I div 4) {$ENDIF};
end;

procedure TBegaCustomComboEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then SetEditRect;
end;

procedure TBegaCustomComboEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FButton.Enabled := Enabled;
end;

procedure TBegaCustomComboEdit.CMEnter(var Message: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TBegaCustomComboEdit.CNCtlColor(var Message: TMessage);
var
  TextColor: Longint;
  BackColor: Longint;
begin
  inherited;
  if NewStyleControls then
  begin
{$R-}
    TextColor := ColorToRGB(Font.Color);
    BackColor := ColorToRGB(Color);
    if not Enabled and (BackColor <> ColorToRGB(clGrayText)) then
      TextColor := ColorToRGB(clGrayText);
    SetTextColor(Message.WParam, TextColor);
{$R+}
  end;
end;

//- BG ----------------------------------------------------------- 12.02.2006 --
procedure TBegaCustomComboEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FDropDown <> nil then
    FDropDown.BiDiMode := BiDiMode;
end;


function TBegaCustomComboEdit.BtnWidthStored: Boolean;
begin
  if (FGlyphKind = gkDefault) and (Glyph <> nil) then
    Result := ButtonWidth <> Max(Glyph.Width div FButton.NumGlyphs + 6,
      DefEditBtnWidth)
  else if FGlyphKind = gkDropDown then
    Result := ButtonWidth <> GetSystemMetrics(SM_CXVSCROLL)
      {$IFNDEF WIN32} + 1{$ENDIF}
  else Result := ButtonWidth <> DefEditBtnWidth;
end;

function TBegaCustomComboEdit.IsCustomGlyph: Boolean;
begin
  Result := FGlyphKind = gkCustom;
end;

procedure TBegaCustomComboEdit.SetGlyphKind(Value: TGlyphKind);
begin
  if FGlyphKind <> Value then begin
    FGlyphKind := Value;
    if (FGlyphKind = gkCustom) and (csReading in ComponentState) then begin
      Glyph := nil;
    end;
    RecreateGlyph;
    if (FGlyphKind = gkDefault) and (Glyph <> nil) then
      ButtonWidth := Max(Glyph.Width div FButton.NumGlyphs + 6, FButton.Width)
    else if FGlyphKind = gkDropDown then begin
      ButtonWidth := GetSystemMetrics(SM_CXVSCROLL){$IFNDEF WIN32} + 1{$ENDIF};
      with FButton do
        ControlStyle := ControlStyle + [csFixedWidth];
    end;
  end;
end;

function TBegaCustomComboEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  Result := nil;
end;

procedure TBegaCustomComboEdit.RecreateGlyph;

  function CreateEllipsisGlyph: TBitmap;
  var
    W, G, I: Integer;
  begin
    Result := TBitmap.Create;
    with Result do
    try
      Monochrome := True;
      Width := Max(1, FButton.Width - 6);
      Height := 4;
      W := 2;
      G := (Result.Width - 3 * W) div 2;
      if G <= 0 then G := 1;
      if G > 3 then G := 3;
      I := (Width - 3 * W - 2 * G) div 2;
      PatBlt(Canvas.Handle, I, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + G + W, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + 2 * G + 2 * W, 1, W, W, BLACKNESS);
    except
      Free;
      raise;
    end;
  end;

var
  NewGlyph: TBitmap;
  DestroyNeeded: Boolean;
begin
  case FGlyphKind of
    gkDefault:
      begin
        DestroyNeeded := False;
        NewGlyph := GetDefaultBitmap(DestroyNeeded);
        try
          FButton.Glyph.Assign(NewGlyph);
          NumGlyphs := FDefNumGlyphs;
        finally
          if DestroyNeeded then NewGlyph.Destroy;
        end;
      end;
    gkDropDown:
      begin
        FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
        NumGlyphs := 1;
      end;
    gkEllipsis:
      begin
        NewGlyph := CreateEllipsisGlyph;
        try
          FButton.Glyph := NewGlyph;
          NumGlyphs := 1;
        finally
          NewGlyph.Destroy;
        end;
      end;
  end;
end;

//- BG ----------------------------------------------------------- 13.02.2006 --
procedure TBegaCustomComboEdit.setComboOptions(
  const Value: TBegaCustomComboEditOptions);
var
  ChangedOptions: TBegaCustomComboEditOptions;
begin
  if FComboOptions <> Value then
  begin
    ChangedOptions := FComboOptions + Value - (FComboOptions * Value);
    FComboOptions := Value;
    doChangedComboOptions(ChangedOptions);
  end;
end;

//- BG ----------------------------------------------------------- 13.02.2006 --
procedure TBegaCustomComboEdit.doChangedComboOptions(
  ChangedOptions: TBegaCustomComboEditOptions);
begin
  if assigned(FOnChangedComboOptions) then
    FOnChangedComboOptions(self, ChangedOptions);
end;

{ TBegaCustomCombobox }

//-- BG ---------------------------------------------------------- 18.05.2009 --
procedure TBegaCustomCombobox.ClearItems;
begin
  if Items.Count > 0 then
  begin
    Items.Clear;
    include(FState, cbsItemsModified);
  end;
end;

//-- BG ---------------------------------------------------------- 18.05.2009 --
constructor TBegaCustomCombobox.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := CBegaCustomComboboxOptionsDefault;
end;

//-- BG ---------------------------------------------------------- 18.05.2009 --
procedure TBegaCustomCombobox.AddItem(Item: string; AObject: TObject);
begin
  inherited;
  include(FState, cbsItemsModified);
end;

//-- BG ---------------------------------------------------------- 18.05.2009 --
procedure TBegaCustomCombobox.BeforeDestruction;
begin
  if not (csDesigning in ComponentState) then
    if cboAutoSaveItems in FOptions then
      if cbsItemsModified in FState then
        SaveItems;
  inherited;
end;

//-- BG ---------------------------------------------------------- 16.05.2009 --
procedure TBegaCustomCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  case Key of
    46: // Entf
    begin
      if DroppedDown then
      begin
        RemoveItem(ItemIndex);
        Key := 0;
      end;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 18.05.2009 --
procedure TBegaCustomCombobox.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    LoadItems;
end;

//-- BG ---------------------------------------------------------- 18.05.2009 --
procedure TBegaCustomCombobox.LoadItems;
begin
  if assigned(FOnLoadItems) then
    FOnLoadItems(self);
end;

//-- BG ---------------------------------------------------------- 16.05.2009 --
procedure TBegaCustomCombobox.RemoveItem(Index: Integer);
begin
  if Index >= 0 then
  begin
    Items.Delete(Index);
    include(FState, cbsItemsModified);
    if Index < Items.Count then
      Text := Items[Index]
    else if Index > 0 then
      Text := Items[Index - 1]
    else
      Text := '';
  end;
end;

//-- BG ---------------------------------------------------------- 18.05.2009 --
procedure TBegaCustomCombobox.SaveItems;
begin
  exclude(FState, cbsItemsModified);
  if assigned(FOnSaveItems) then
    FOnSaveItems(self);
end;

//-- BG ---------------------------------------------------------- 16.05.2009 --
function TBegaCustomCombobox.UpdateItem(const Item: string; const AObject: TObject): Integer;
begin
  Result := Items.IndexOf(Item);
  if Result < 0 then
  begin
    Result := 0;
    Items.InsertObject(Result, Item, AObject);
    include(FState, cbsItemsModified);
  end
  else if Items.Objects[Result] <> AObject then
  begin
    Items.Objects[Result] := AObject;
    include(FState, cbsItemsModified);
  end;
end;

{ TBegaComboBox }

//-- BG ---------------------------------------------------------- 16.05.2009 --
procedure TBegaComboBox.AfterConstruction;
begin
  inherited;
  FRegistryValuePrefix := 'Item';
end;

//-- BG ---------------------------------------------------------- 16.05.2009 --
procedure TBegaComboBox.LoadFromRegistry(const RegistryKey, ValuePrefix: String);
var
  Registry: TRegistry;
  Values: TStringList;
  Index, Len: Integer;
  Prefix: String;
begin
  Prefix := ValidateValuePrefix(ValuePrefix);
  Len := Length(Prefix);
  Values := TStringList.create;
  try
    Registry := TRegistry.Create;
    try
      Registry.OpenKey(ValidateRegistryKey(RegistryKey), False);
      Registry.GetValueNames(Values);
      for Index := Values.Count - 1 downto 0 do
        if SameText(Copy(Values[Index], 1, Len), Prefix) then
          Values[Index] := Registry.ReadString(Values[Index])
        else
          Values.delete(Index);
      Registry.CloseKey;
      Items.Assign(Values);
    finally
      Registry.free;
    end;
  finally
    Values.free;
  end;
end;

//-- BG ---------------------------------------------------------- 18.05.2009 --
procedure TBegaComboBox.LoadItems;
begin
  inherited;
  if (FRegistryKey <> '') and (FRegistryValuePrefix <> '') then
    LoadFromRegistry(FRegistryKey, FRegistryValuePrefix);
end;

//-- BG ---------------------------------------------------------- 18.05.2009 --
procedure TBegaComboBox.SaveItems;
begin
  inherited;
  if (FRegistryKey <> '') and (FRegistryValuePrefix <> '') then
    SaveToRegistry(FRegistryKey, FRegistryValuePrefix);
end;

//-- BG ---------------------------------------------------------- 16.05.2009 --
procedure TBegaComboBox.SaveToRegistry(const RegistryKey, ValuePrefix: String);
var
  Registry: TRegistry;
  Index: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    Registry := TRegistry.Create;
    try
      Registry.OpenKey(ValidateRegistryKey(RegistryKey), True);
      Registry.GetValueNames(Values);
      for Index := 0 to Items.Count - 1 do
        Registry.WriteString(ValidateValueName(ValuePrefix, Index), Items[Index]);
      for Index := Items.Count to Values.Count - 1 do
        Registry.DeleteValue(ValidateValueName(ValuePrefix, Index));
      Registry.CloseKey;
    finally
      Registry.free;
    end;
  finally
    Values.free;
  end;
end;

//-- BG ---------------------------------------------------------- 16.05.2009 --
function TBegaComboBox.ValidateRegistryKey(const RegistryKey: String): String;
begin
  if RegistryKey <> '' then
    Result := RegistryKey
  else if FRegistryKey <> '' then
    Result := FRegistryKey
  else
    raise EBegaNotAssignedException.Create(ClassName + ' ''' + Name + ''': required RegistryKey not assigned.');
end;

//-- BG ---------------------------------------------------------- 16.05.2009 --
function TBegaComboBox.ValidateValueName(const ValuePrefix: String; Index: Integer): String;
begin
  Result := ValidateValuePrefix(ValuePrefix) + IntToStr(Index);
end;

//-- BG ---------------------------------------------------------- 16.05.2009 --
function TBegaComboBox.ValidateValuePrefix(const ValuePrefix: String): String;
begin
  if ValuePrefix <> '' then
    Result := ValuePrefix
  else if FRegistryValuePrefix <> '' then
    Result := FRegistryValuePrefix
  else
    raise EBegaNotAssignedException.Create(ClassName + ' ''' + Name + ''': required RegistryValuePrefix not assigned.')
end;

end.
