{-------------------------------------------------------------------------------
$File$, (C) 2006 by Bernd Gabriel. All rights reserved.
--------------------------------------------------------------------------------
Author       : $Author: Bernd $
Last CheckIn : $Date: 2007/09/02 21:06:55 $
Revision     : $Revision: 1.4 $

mailto: info@fast-function-factory.de

Log:
====

$Log: BegaVirtualTrees.pas,v $
Revision 1.4  2007/09/02 21:06:55  Bernd
curious redirections (code 301) caused by error in BegaFormViewer:
NewUrl and thus new UrlBase were not set properly in FrameBrowserGetPostRequest.

Revision 1.3  2007/09/01 15:16:41  Bernd
D11 warnings and hints removed

Revision 1.2  2006/08/31 20:59:58  Bernd
no message

Revision 1.1  2006/02/21 20:24:46  B.Gabriel
new: BegaComboBox, BegaVSTComboBox

-------------------------------------------------------------------------------}

unit BegaVirtualTrees;

interface

uses
  Windows,
  Classes,
  Controls,
  SysUtils,
  VirtualTrees,
  VTEditors;

type
  //----------------------------------------------------------------------------
  // BG, 15.05.2005: TBegaControlEditLink
  //----------------------------------------------------------------------------
  // thanks to Werner Lehmann, 13.02.2004
  //----------------------------------------------------------------------------
  TBegaEditStringEvent = procedure(ASender: TObject; var AString: WideString) of object; // BG, 13.05.2004

  TBegaControlEditLink = class(TCustomEditLink)
  private
    FControl: TWinControl;
    FControlParent: TWinControl;
    FOnSetEditText: TBegaEditStringEvent;
    procedure setEdit(const Value: TWinControl);
  protected
    function CreateEditControl: TWinControl; override;
    procedure FreeEditControl; override;
    procedure SetEditText(const Value: WideString); override; // BG, 13.05.2004
    procedure StopEdit; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy(); override;
    property EditControl: TWinControl read FControl write setEdit;
  published
    property OnSetEditText: TBegaEditStringEvent read FOnSetEditText write FOnSetEditText;
  end;

  //----------------------------------------------------------------------------
  // BG, 22.05.2010: TBegaCustomVirtualStringTree extracted from TBegaVirtualStringTree
  //----------------------------------------------------------------------------
  TBegaCustomVirtualStringTree = class(TVirtualStringTree)
  private
    FControlEditLink: TBegaControlEditLink;
    function GetControlEditLink: TBegaControlEditLink;
    function GetNodeOfData(Data: TObject): PVirtualNode;
  protected
    function DoCompare(Node1: PVirtualNode; Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function GetObject(Node: PVirtualNode): TObject; virtual;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    procedure SetObject(Node: PVirtualNode; const Value: TObject); virtual;
    property ControlEditLink: TBegaControlEditLink read GetControlEditLink;
    property NodeOfData[Data: TObject]: PVirtualNode read GetNodeOfData;
    property Objects[Node: PVirtualNode]: TObject read GetObject write SetObject;
  end;

  //----------------------------------------------------------------------------
  // BG, 16.02.2006: TBegaVirtualStringTree
  //----------------------------------------------------------------------------
  TBegaVirtualStringTree = class(TBegaCustomVirtualStringTree)
  public
    property NodeOfData;
    property Objects;
  end;

implementation

{ TBegaControlEditLink }

//- WL -------------------------------------------------------------------------
constructor TBegaControlEditLink.create(AOwner: TPersistent);
begin
  inherited;
  // just for debugging
end;

//- WL -------------------------------------------------------------------------
function TBegaControlEditLink.createEditControl: TWinControl;
begin
  Result := FControl;
  FControlParent := EditControl.Parent;
end;

//- WL -------------------------------------------------------------------------
destructor TBegaControlEditLink.destroy;
begin
  EditControl := nil;
  inherited;
end;

//- WL -------------------------------------------------------------------------
procedure TBegaControlEditLink.freeEditControl;
begin
  // do not free control, is owned by someone else: // inherited freeEditControl;
end;

//- WL -------------------------------------------------------------------------
procedure TBegaControlEditLink.setEdit(const Value: TWinControl);
begin
  if (FControl <> nil) and (Owner is TComponent) then
    FControl.removeFreeNotification(Owner as TComponent);

  FControl := Value;
  if (FControl <> nil) and (Owner is TComponent) then
    FControl.freeNotification(Owner as TComponent);
end;

//- BG ----------------------------------------------------------- 13.05.2004 --
procedure TBegaControlEditLink.setEditText(const Value: WideString);
var
  Text: WideString;
begin
  Text := Value;
  if assigned(FOnSetEditText) then
    FOnSetEditText(FControl, Text);
  inherited setEditText(Text);
end;

//- WL -------------------------------------------------------------------------
procedure TBegaControlEditLink.stopEdit;
begin
  inherited;
  EditControl.Parent := FControlParent;
end;

{ TBegaCustomVirtualStringTree }

//- BG ----------------------------------------------------------- 16.02.2006 --
function TBegaCustomVirtualStringTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
begin
  Result := ansiCompareText(Text[Node1, Column], Text[Node2, Column]);
end;

//- BG ----------------------------------------------------------- 16.02.2006 --
procedure TBegaCustomVirtualStringTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  with Header do
  begin
    if SortColumn = -1 then
    begin
      SortDirection := sdAscending;
      SortColumn := HitInfo.Column;
    end
    else if SortColumn = HitInfo.Column then
      case SortDirection of
        sdAscending: SortDirection := sdDescending;
        sdDescending: SortColumn := -1;
      end
    else
      SortColumn := HitInfo.Column;
    Treeview.SortTree(SortColumn, SortDirection);
  end;
end;

//- WL -------------------------------------------------------------------------
function TBegaCustomVirtualStringTree.GetControlEditLink: TBegaControlEditLink;
begin
  if FControlEditLink = nil then
    FControlEditLink := TBegaControlEditLink.create(Self);
  Result := FControlEditLink;
end;

//- BG ----------------------------------------------------------- 31.08.2006 --
function TBegaCustomVirtualStringTree.GetNodeOfData(Data: TObject): PVirtualNode;
begin
  Result := getFirst;
  while Result <> nil do
  begin
    if Objects[Result] = Data then
      break;
    Result := getNext(Result);
  end;
end;

//- BG ----------------------------------------------------------- 16.02.2006 --
function TBegaCustomVirtualStringTree.GetObject(Node: PVirtualNode): TObject;
begin
  assert(NodeDataSize >= sizeof(TObject));
  Result := TObject(GetNodeData(Node)^);
end;

//- BG ----------------------------------------------------------- 19.02.2006 --
procedure TBegaCustomVirtualStringTree.SetObject(Node: PVirtualNode;
  const Value: TObject);
begin
  assert(NodeDataSize >= sizeof(TObject));
  TObject(GetNodeData(Node)^) := Value;
end;

end.
 