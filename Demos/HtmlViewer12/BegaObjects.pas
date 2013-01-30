{-------------------------------------------------------------------------------
BegaObjects.pas, (C) 2004-2010 by Bernd Gabriel. All rights reserved.

mailto: info@fast-function-factory.de
-------------------------------------------------------------------------------}

unit BegaObjects;

//{$define BEGA_REF_COUNTING}

interface

uses
  Classes, Contnrs, Db, SysUtils, TypInfo;

resourcestring
  CBegaAssignException = 'Cannot assign a %s to a %s';
  CBegaEndUpdateError = ': endUpdate without matching beginUpdate.';
  CBegaExceptionFieldOutOfRange = ': field out of range';
  CBegaExceptionIndexOutOfRange = ': index out of range';
  CBegaGetIndexWhileUpdating = ': cannot use index while updating.';
  CBegaNotAssignedException = 'Cannot assign a %s to a %s';

const
  CBegaIndexByPointer = 0;
  CBegaIndexByKey = 1;
  CBegaIndexByName = 2;

type

//------------------------------------------------------------------------------
// BG, 10.12.2004: EBegaException: base exception for all exceptions introduced in bega code.
//------------------------------------------------------------------------------

  EBegaException = class(Exception);

  EBegaAssignException = class(EBegaException);
  EBegaEndUpdateError = class(EBegaException);
  EBegaEnumException = class(EBegaException)
  public
    constructor createEnum(EnumType: PTypeInfo; const Value);
    constructor createOrd(EnumType: PTypeInfo; Value: Integer);
  end;
  EBegaListException = class(EBegaException)
  public
    constructor createNotInList(ObjectType, ObjectName: String);
  end;
  EBegaNotAssignedException = class(EBegaException);
  EBegaRangeException = class(EBegaException);

//------------------------------------------------------------------------------
// BG, 28.08.2007: TBegaThread: just a wrapper at the moment
//------------------------------------------------------------------------------

  TBegaThread = class(TThread);

//------------------------------------------------------------------------------
// BG, 10.12.2004: TBegaObject: base class for all common BegaObjects
//------------------------------------------------------------------------------
// BG, 10.12.2004: IBegaObject: base interface for all common BegaObjects
//------------------------------------------------------------------------------

  TBegaObject = class;

  IBegaObject = interface
    ['{CB0B48A2-6D84-43C2-99F0-A94EEDE69D8C}']
    function BegaObject: TBegaObject;
  end;

  TBegaObject = class(TInterfacedObject, IBegaObject) // BG, 04.12.2004: becomes interfaced object
  protected
    function AssignException(ASource, ADest: TObject): EBegaAssignException;
    function BegaObject: TBegaObject;
    // load/save properties to/from current record of dataset
    procedure doLoadShallow(Dataset: TDataset); virtual; // called by loadShallow()
    procedure doSaveShallow(Dataset: TDataset); virtual; // called by saveShallow()
    procedure loadShallow(Dataset: TDataset); virtual; // called by assign(), if Source is TDataset
    procedure saveShallow(Dataset: TDataset); virtual; // called by assignTo(), if Dest is TDataset
  public
{$ifdef BEGA_REF_COUNTING}
    constructor Create;
    destructor Destroy; override;
{$endif BEGA_REF_COUNTING}
    // allow using BegaObjects directly / without using interfaces
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    // assign data from one object to another: if source is a TBegaObject and
    // assign() does not implement the assignment, this base implementation tries
    // source.assignTo(self). Therefore descendents trial to dest.assign(self)
    // in assignTo() probably will end up in a stack overflow!
    procedure Assign(Source: TObject); virtual;
    procedure AssignTo(Dest: TObject); virtual;
    function ToString: String; virtual;
  end;
  //TBegaObjectClass = class of TBegaObject;

//------------------------------------------------------------------------------
// BG, 18.12.2004: list stuff
//------------------------------------------------------------------------------

  TBegaCompareEvent = function (Item1, Item2: Pointer): Integer of object;
  TBegaListClass = class of TList;
  TBegaListNotification = TListNotification;
  TBegaListNotifyEvent = procedure(Sender: TList; Ptr: Pointer; Action: TBegaListNotification) of object;

//  IBegaIterator = interface
//    ['{B0BA68BE-4D57-4A6F-9D2F-589FAA506AEA}']
//    function getNext: Pointer;
//    function hasNext: Boolean;
//    property Next: Pointer read getNext;
//  end;

//------------------------------------------------------------------------------
// BG, 18.12.2004: TBegaList: TList with notification event
//------------------------------------------------------------------------------

  TBegaList = class(TList)
  private
    FOnChange: TBegaListNotifyEvent;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure Add(List: TList); overload;
    procedure Sort(compare: TBegaCompareEvent); overload;
    property OnChange: TBegaListNotifyEvent read FOnChange write FOnChange;
  end;

//------------------------------------------------------------------------------
// BG, 18.12.2004: TBegaObjectList: TObjectList with notification event
//------------------------------------------------------------------------------

  TBegaObjectList = class(TObjectList)
  private
    FOnChange: TBegaListNotifyEvent;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure Sort(compare: TBegaCompareEvent); overload;
    property OnChange: TBegaListNotifyEvent read FOnChange write FOnChange;
  end;

//------------------------------------------------------------------------------
// BG, 11.06.2005: TBegaInterfaceList: TInterfaceList without notification event
//  since TInterfaceList does not support Notify().
//------------------------------------------------------------------------------

  TBegaInterfaceList = class(TInterfaceList);

////------------------------------------------------------------------------------
//// BG, 10.03.2007: TBegaIterator: iterating and pointer collection
////------------------------------------------------------------------------------
//
//  //BG, 10.03.2007:
//  TBegaIteratorStatus = (isGetFirst, isGetNext, isHasNext, isDone, isCalledNextAfterDoneError);
//
//  //BG, 10.03.2007:
//  //BG, 07.02.2009: reference counted, interfaced object. Do not reference as object!
//  TBegaIterator = class(TInterfacedObject, IBegaIterator)
//  private
//    FStatus: TBegaIteratorStatus;
//    FNext: Pointer;
//    function getNext: Pointer;
//  protected
//    procedure doGetFirst(var Next: Pointer); virtual; abstract;
//    procedure doGetNext(var Next: Pointer); virtual; abstract;
//  public
//    function hasNext: Boolean;
//    property Next: Pointer read getNext;
//  end;
//
//  //BG, 10.03.2007: the list iterator as an easy example for an iterator
//  TBegaListIterator = class(TBegaIterator)
//  private
//    FList: TList;
//    FIndex: Integer;
//  protected
//    procedure doGetFirst(var Next: Pointer); override;
//    procedure doGetNext(var Next: Pointer); override;
//  public
//    constructor create(List: TList);
//  end;

//------------------------------------------------------------------------------
// BG, 11.06.2005: TBegaMessage is the message sent by IBegaSenders
//------------------------------------------------------------------------------
// The message may contain fields or methods to render the message more precisely.
// The type of the message may be the message itself.
//
// The message may contain fields or methods to reply.
// There are no prescriptions, how to implement the reply mechanism right now.
//------------------------------------------------------------------------------

  TBegaMessage = class(TBegaObject);
  TBegaSenderDestroyedMessage = class(TBegaMessage);

//------------------------------------------------------------------------------
// BG, 11.06.2005: IBegaReceiver receives messages from IBegaSenders.
//------------------------------------------------------------------------------

  IBegaReceiver = interface
    ['{662DBAE5-9377-4D15-B6A7-DB9F4196CDB5}']
    procedure receiveMessage(ASender: TObject; AMessage: TBegaMessage);
  end;

//------------------------------------------------------------------------------
// BG, 11.06.2005: TBegaSender: maintains a list of receivers and sends messages to them
//------------------------------------------------------------------------------

  TBegaSender = class(TBegaObject)
  private
    FReceivers: TInterfaceList;
    function getReceiver(AIndex: Integer): IBegaReceiver;
    function getReceiverCount: Integer;
  protected
    property ReceiverCount: Integer read getReceiverCount;
    property Receivers[AIndex: Integer]: IBegaReceiver read getReceiver; default;
  public
    constructor Create;
    destructor Destroy; override;
    procedure registerReceiver(AReceiver: IBegaReceiver);
    procedure sendMessage(ASender: TObject; AMessage: TBegaMessage; AFreeMessage: Boolean = true);
    procedure unregisterReceiver(AReceiver: IBegaReceiver);
  end;

//------------------------------------------------------------------------------
// BG, 29.11.2004: TBegaItem: base class for all objects, which may be held in containers
//   - it has an owner, which it notifies, if its state changes
//   - it can compare itself with other TBegaItems
//------------------------------------------------------------------------------
// The container may use compare() for sorting and binarily searching items
// index 0: Address/Pointer/Reference
//------------------------------------------------------------------------------

  TBegaItemState = (
    // notified states
    biObjectLoaded,   // object has been loaded. is sent instead of multiple biDataChanged.
    biDataChanged,    // data of object has been changed. is sent, when dirty flag is set.
    biOwnerChanging,  // owner is going to change. is sent to current owner, which has to extract object from its item list.
    // unnotified states
    biObjectLoading, biDataChanging, biDataChangePending);
  TBegaItemStateSet = set of TBegaItemState;
  TBegaItemNotification = biObjectLoaded..biOwnerChanging;

  TBegaItemOwner = class;

  TBegaItem = class(TBegaObject)
  private
    FOwner: TBegaItemOwner;
    FState: TBegaItemStateSet;
    function getState(State: TBegaItemState): Boolean;
    procedure setOwner(const Value: TBegaItemOwner); // extracts item from current owner
    procedure setState(State: TBegaItemState; Value: Boolean = TRUE);
    function getDirty: Boolean;
    procedure setDirty(Value: Boolean);
  protected
    // override this to implement the indexes you want to offer
    // if an index is compound of several fields, the Fields parameter tells us,
    // how any of these fields to compare.
    // Example: may index 5 support 3 fields (Name, FirstName, MidName)
    //   docompare(WithThis, 5, 2) skips comparing MidName, thus Robert A. Miller equals Robert Z. Miller
    function doCompare(WithObject: TBegaItem; Comparison, Fields: Integer): Integer; virtual;
    // state change after loading/saving
    procedure loadShallow(Dataset: TDataset); override; // called by assign(), if Source is TDataset
    procedure saveShallow(Dataset: TDataset); override; // called by assignTo(), if Dest is TDataset
    // item owner. If you set a State, the owner gets notified
    procedure notifyOwner(ANotification: TBegaItemNotification);
    // item state. If you set a State, the owner gets notified
    property ItemState: TBegaItemStateSet read FState;
    property Loaded: Boolean index biObjectLoaded read getState write setState;
  public
    // override this to tell the world, how many comparisons doCompare() offers
    class function ComparisonCount: Integer; virtual; // index 0: address/pointer
    // override this to tell the world, how many fields the specified comparisons of doCompare() supports
    class function FieldCountOfComparison(Comparison: Integer): Integer; virtual;
    // constructor must be virtual for class factory / object creation via TBegaItemClass
    constructor Create; virtual; // needs to be virtual for creation via TBegaItemClass
    destructor Destroy; override;  // extracts item from current owner
    // compare with object WithObject. use index Comparison. If it is a compound index,
    // compare up to Fields fields of index. If Fields = -1 all fields are compared
    function compare(WithObject: TBegaItem; Comparison: Integer; Fields: Integer = -1): Integer;
    //
    procedure beginUpdate; // sets state biDataChanging
    procedure endUpdate;   // resets state biDataChanging and notifies datachanges is biDataChangePending
    //
    property Owner: TBegaItemOwner read FOwner write setOwner;
    property Dirty: Boolean read getDirty write setDirty;
  end;
  TBegaItemClass = class of TBegaItem;

//------------------------------------------------------------------------------
// BG, 11.06.2005: TBegaItemOwner: base class for all item owners
//------------------------------------------------------------------------------

  TBegaItemOwner = class(TBegaItem)
  protected
    // onItemNotification() is called by an item to notify its owner
    procedure onItemNotification(ASender: TBegaItem; ANotification: TBegaItemNotification); virtual;
  end;

//------------------------------------------------------------------------------
// BG, 10.05.2005: TBegaCustomNameKeyObject: can be held in TBegaCustomNameKeyContainer
//------------------------------------------------------------------------------
// In addition to index 0: Address/Pointer/Reference defined in
// TBegaIndexedObject TBegaCustomNameKeyObject defines:
// index 1: Key
// index 2: Name
//
// both name and key are abtract
//------------------------------------------------------------------------------

  TBegaKey = Integer;
  TBegaKeyRange = record First, Last: TBegaKey; end;
  TBegaKeyArray = array of TBegaKey;

  TBegaName = WideString;

  TBegaCustomNameKeyObject = class(TBegaItem)
  private
    function getKey: TBegaKey; virtual; abstract;
    function getName: TBegaName; virtual; abstract;
    procedure setKey(const Key: TBegaKey); virtual; abstract;
    procedure setName(const Name: TBegaName); virtual; abstract;
  protected
    class function NewKey: TBegaKey;
    function doCompare(WithObject: TBegaItem; Comparison, Fields: Integer): Integer; override;
  public
    class function ComparisonCount: Integer; override;// index 1: Key, index 2: Name
    class function FieldCountOfComparison(Comparison: Integer): Integer; override;
    //
    property Dirty;
    property ItemState;
    property Loaded;
    //
    property Key: TBegaKey read getKey write setKey;
    property Name: TBegaName read getName write setName;
  end;

//------------------------------------------------------------------------------
// BG, 10.05.2005: TBegaKeyObject: can be held in TBegaCustomNameKeyContainer
//------------------------------------------------------------------------------
// name is abstract, a key is held inside
//------------------------------------------------------------------------------

  TBegaKeyObject = class(TBegaCustomNameKeyObject)
  private
    FKey: TBegaKey;
  protected
    function getKey: TBegaKey; override;
    procedure setKey(const Key: TBegaKey); override;
    function getName: TBegaName; override;
    procedure setName(const Name: TBegaName); override;
  public
    constructor Create; override;
  end;

//------------------------------------------------------------------------------
// BG, 10.05.2005: TBegaNameObject: can be held in TBegaCustomNameKeyContainer
//------------------------------------------------------------------------------
// key is abstract, the name is held inside
//------------------------------------------------------------------------------

  TBegaNameObject = class(TBegaCustomNameKeyObject)
  private
    FName: TBegaName;
  protected
    function getKey: Integer; override;
    procedure setKey(const Key: Integer); override;
    function getName: TBegaName; override;
    procedure setName(const Name: TBegaName); override;
  end;

//------------------------------------------------------------------------------
// BG, 10.05.2005: TBegaNameKeyObject: can be held in TBegaCustomNameKeyContainer
//------------------------------------------------------------------------------
// both name and key are held inside
//------------------------------------------------------------------------------

  TBegaNameKeyObject = class(TBegaCustomNameKeyObject)
  private
    FKey: TBegaKey;
    FName: TBegaName;
  protected
    function getKey: Integer; override;
    procedure setKey(const Key: Integer); override;
    function getName: TBegaName; override;
    procedure setName(const Name: TBegaName); override;
  public
    constructor Create; override;
    constructor createNew(NamePrefix: String);
  end;

//------------------------------------------------------------------------------

function comparePointers(Item1, Item2: Pointer): Integer;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation

{$ifdef BEGA_REF_COUNTING}

var
  VObjectCounter: TStringList;

//- BG ----------------------------------------------------------- 29.11.2004 --
procedure addRef(AObject: TObject; AInc: Integer);
var
  Index: Integer;
begin
  if VObjectCounter = nil then
    VObjectCounter := TStringList.create;
  Index := VObjectCounter.indexOf(AObject.ClassName);
  if Index >= 0 then
    VObjectCounter.Objects[Index] := TObject(Integer(VObjectCounter.Objects[Index]) + AInc)
  else
    VObjectCounter.addObject(AObject.ClassName, TObject(AInc))
end;

{$endif BEGA_REF_COUNTING}

//- BG ----------------------------------------------------------- 10.10.2006 --
function comparePointers(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(Item1) - Integer(Item2);
end;


procedure quickSort(SortList: PPointerList; L, R: Integer; compare: TBegaCompareEvent);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while compare(SortList^[I], P) < 0 do
        Inc(I);
      while compare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, compare);
    L := I;
  until I >= R;
end;

{ EBegaEnumException }

//- BG ----------------------------------------------------------- 23.01.2005 --
constructor EBegaEnumException.createEnum(EnumType: PTypeInfo; const Value);
begin
  inherited createFmt('unsupported: %s.%s', [EnumType.Name, GetEnumName(EnumType, integer(Value))]);
end;

//- BG ----------------------------------------------------------- 14.08.2006 --
constructor EBegaEnumException.createOrd(EnumType: PTypeInfo; Value: Integer);
begin
  inherited createFmt('unsupported: %s: %d', [EnumType.Name, Value]);
end;

{ EBegaListException }

//- BG ----------------------------------------------------------- 25.09.2005 --
constructor EBegaListException.createNotInList(ObjectType,
  ObjectName: String);
begin
  inherited createFmt('%s ''%s'' not in list', [ObjectType, ObjectName]);
end;

{ TBegaObject }

//- BG ----------------------------------------------------------- 04.12.2004 --
procedure TBegaObject.afterConstruction;
begin
  // BG, 04.12.2004: do not call inherited; it decrements the refcount,
  // but we want to access the object by object and not only by interface.
end;

//- BG ----------------------------------------------------------- 04.12.2004 --
procedure TBegaObject.assign(Source: TObject);
begin
  if Source is TBegaObject then
    TBegaObject(Source).assignTo(self)
  else if Source is TDataset then
    loadShallow(TDataset(Source))
  else
    raise AssignException(Source, self);
end;

//- BG ----------------------------------------------------------- 04.12.2004 --
function TBegaObject.AssignException(ASource, ADest: TObject): EBegaAssignException;
var
  SrcName, DstName: String;
begin
  if ASource <> nil then
    SrcName := ASource.ClassName
  else
    SrcName := 'nil';
  if ADest <> nil then
    DstName := ADest.ClassName
  else
    DstName := 'nil';
  Result := EBegaAssignException.CreateFmt(CBegaAssignException, [SrcName, DstName]);
end;

//- BG ----------------------------------------------------------- 04.12.2004 --
procedure TBegaObject.assignTo(Dest: TObject);
begin
  if Dest is TDataset then
    saveShallow(TDataset(Dest))
  else
    raise AssignException(self, Dest);
end;

//- BG ----------------------------------------------------------- 04.12.2004 --
procedure TBegaObject.BeforeDestruction;
begin
  inherited afterConstruction; // decrement the refcount now
  inherited beforeDestruction;
end;

//- BG ----------------------------------------------------------- 05.12.2004 --
function TBegaObject.BegaObject: TBegaObject;
begin
  Result := self;
end;

{$ifdef BEGA_REF_COUNTING}

//- BG ----------------------------------------------------------- 29.11.2004 --
constructor TBegaObject.create;
begin
  inherited;
  addRef(self, 1);
end;

{$endif BEGA_REF_COUNTING}

{$ifdef BEGA_REF_COUNTING}

//- BG ----------------------------------------------------------- 29.11.2004 --
destructor TBegaObject.destroy;
begin
  addRef(self, -1);
  inherited;
end;

{$endif BEGA_REF_COUNTING}

//- BG ----------------------------------------------------------- 12.05.2005 --
procedure TBegaObject.doLoadShallow(Dataset: TDataset);
begin
  // nothing. descendents can load their properties from Dataset
end;

//- BG ----------------------------------------------------------- 12.05.2005 --
procedure TBegaObject.doSaveShallow(Dataset: TDataset);
begin
  // nothing. descendents can save their properties to Dataset
end;

//- BG ----------------------------------------------------------- 12.05.2005 --
procedure TBegaObject.loadShallow(Dataset: TDataset);
begin
  doLoadShallow(Dataset);
end;

//- BG ----------------------------------------------------------- 12.05.2005 --
procedure TBegaObject.saveShallow(Dataset: TDataset);
begin
  doSaveShallow(Dataset);
end;

//-- BG ---------------------------------------------------------- 26.05.2010 --
function TBegaObject.ToString: String;
begin
  Result := ClassName + '@' + Format('%p', [self]);
end;

{ TBegaList }

//- BG ----------------------------------------------------------- 10.10.2006 --
procedure TBegaList.Add(List: TList);
var
  Offset: Integer;
  Index: Integer;
begin
  Offset := Count;
  Count := Offset + List.Count;
  for Index := 0 to List.Count - 1 do
    Items[Offset + Index] := List[Index];
end;

//- BG ----------------------------------------------------------- 18.12.2004 --
procedure TBegaList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if assigned(FOnChange) then
    FOnChange(self, Ptr, TBegaListNotification(Action));
  // execute FOnChange before inherited Notify(), for compatibility to TBegaObjectList.
  inherited;
end;

//- BG ----------------------------------------------------------- 26.09.2006 --
procedure TBegaList.Sort(compare: TBegaCompareEvent);
begin
  if (List <> nil) and (Count > 1) then
    QuickSort(List, 0, Count - 1, compare);
end;

{ TBegaObjectList }

//- BG ----------------------------------------------------------- 18.12.2004 --
procedure TBegaObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if assigned(FOnChange) then
    FOnChange(self, Ptr, TBegaListNotification(Action));
  // must execute FOnChange before inherited Notify(), since it may free object at Ptr.
  inherited;
end;

//- BG ----------------------------------------------------------- 26.09.2006 --
procedure TBegaObjectList.Sort(compare: TBegaCompareEvent);
begin
  if (List <> nil) and (Count > 1) then
    QuickSort(List, 0, Count - 1, compare);
end;

//{ TBegaIterator }
//
////- BG ----------------------------------------------------------- 10.03.2007 --
//function TBegaIterator.getNext: Pointer;
//begin
//  Result := FNext;
//  case FStatus of
//    isGetNext: ;
//    isHasNext: FStatus := isGetNext;
//    isDone:    FStatus := isCalledNextAfterDoneError;
//    isCalledNextAfterDoneError:
//      raise EBegaRangeException.Create(Classname + '.getNext() called beyond end of iteration.');
//  else
//      raise EBegaEnumException.createEnum(TypeInfo(TBegaIteratorStatus), FStatus);
//  end;
//end;
//
////- BG ----------------------------------------------------------- 10.03.2007 --
//function TBegaIterator.hasNext: Boolean;
//begin
//  if FStatus = isGetNext then
//  begin
//    doGetNext(FNext);
//    if FNext <> nil then
//      FStatus := isHasNext
//    else
//      FStatus := isDone;
//  end;
//  Result := FStatus = isHasNext;
//end;

//{ TBegaListIterator }
//
////- BG ----------------------------------------------------------- 10.03.2007 --
//constructor TBegaListIterator.create(List: TList);
//begin
//  inherited create;
//  FList := List;
//end;
//
////-- BG ---------------------------------------------------------- 12.02.2009 --
//procedure TBegaListIterator.doGetFirst(var Next: Pointer);
//begin
//  FIndex := 0;
//end;
//
////- BG ----------------------------------------------------------- 10.03.2007 --
//procedure TBegaListIterator.doGetNext(var Next: Pointer);
//begin
//  if FIndex < FList.Count then
//  begin
//    Next := FList[FIndex];
//    inc(FIndex);
//  end
//  else
//    Next := nil;
//end;

{ TBegaSender }

//- BG -------------------------------------------------------------------------
constructor TBegaSender.create;
begin
  inherited;
  FReceivers := TInterfaceList.create;
end;

//- BG -------------------------------------------------------------------------
destructor TBegaSender.destroy;
begin
  sendMessage(self, TBegaSenderDestroyedMessage.create);
  FReceivers.free;
  inherited;
end;

//- BG ----------------------------------------------------------- 11.06.2005 --
function TBegaSender.getReceiver(AIndex: Integer): IBegaReceiver;
begin
  Result := FReceivers[AIndex] as IBegaReceiver;
end;

//- BG ----------------------------------------------------------- 11.06.2005 --
function TBegaSender.getReceiverCount: Integer;
begin
  Result := FReceivers.Count;
end;

//- BG ----------------------------------------------------------- 11.06.2005 --
procedure TBegaSender.registerReceiver(AReceiver: IBegaReceiver);
begin
  if FReceivers.indexOf(AReceiver) = -1 then
    FReceivers.add(AReceiver);
end;

//- BG ----------------------------------------------------------- 11.06.2005 --
procedure TBegaSender.sendMessage(ASender: TObject; AMessage: TBegaMessage; AFreeMessage: Boolean);
var
  Index: Integer;
begin
  for Index := 0 to ReceiverCount - 1 do
    Receivers[Index].receiveMessage(ASender, AMessage);
  if AFreeMessage then
    AMessage.free;
end;

//- BG ----------------------------------------------------------- 11.06.2005 --
procedure TBegaSender.unregisterReceiver(AReceiver: IBegaReceiver);
begin
  FReceivers.remove(AReceiver);
end;

{ TBegaItem }

//- BG ----------------------------------------------------------- 26.09.2005 --
procedure TBegaItem.beginUpdate;
begin
  include(FState, biDataChanging);
end;

//- BG ----------------------------------------------------------- 29.11.2004 --
function TBegaItem.compare(WithObject: TBegaItem; Comparison, Fields: Integer): Integer;
var
  FieldCount: Integer;
begin
  if (Comparison < 0) or (Comparison >= ComparisonCount) then
    raise EBegaRangeException.create(ClassName + CBegaExceptionIndexOutOfRange);
  FieldCount := FieldCountOfComparison(Comparison);
  if Fields = -1 then
    Fields := FieldCount;
  if (Fields < 1) or (Fields > FieldCount) then
    raise EBegaRangeException.create(ClassName + CBegaExceptionFieldOutOfRange);
  Result := doCompare(WithObject, Comparison, Fields);
end;

//- BG ----------------------------------------------------------- 29.11.2004 --
class function TBegaItem.ComparisonCount: Integer;
begin
  Result := 1;
end;

//- BG ----------------------------------------------------------- 09.06.2005 --
constructor TBegaItem.create;
begin
  inherited create;
end;

//- BG ----------------------------------------------------------- 14.05.2005 --
destructor TBegaItem.destroy;
begin
  Owner := nil;
  inherited;
end;

//- BG ----------------------------------------------------------- 29.11.2004 --
function TBegaItem.doCompare(WithObject: TBegaItem; Comparison, Fields: Integer): Integer;
begin
  // just a simple example, how to use the indexes: compare address/pointer
  case Comparison of
    CBegaIndexByPointer: Result := integer(self) - integer(WithObject);
  else
    raise EBegaRangeException.create(ClassName + CBegaExceptionIndexOutOfRange);
  end;
end;

//- BG ----------------------------------------------------------- 26.09.2005 --
procedure TBegaItem.endUpdate;
begin
  exclude(FState, biDataChanging);
  if biDataChangePending in FState then
  begin
    exclude(FState, biDataChangePending);
    Dirty := TRUE;
  end;
end;

//- BG ----------------------------------------------------------- 29.11.2004 --
class function TBegaItem.FieldCountOfComparison(Comparison: Integer): Integer;
begin
  case Comparison of
    CBegaIndexByPointer: Result := 1;
  else
    raise EBegaException.create(ClassName + CBegaExceptionIndexOutOfRange);
  end;
end;

//- BG ----------------------------------------------------------- 26.09.2005 --
function TBegaItem.getDirty: Boolean;
begin
  Result := FState * [biDataChanged, biDataChangePending] <> [];
end;

//- BG ----------------------------------------------------------- 14.05.2005 --
function TBegaItem.getState(State: TBegaItemState): Boolean;
begin
  Result := State in FState;
end;

//- BG ----------------------------------------------------------- 14.05.2005 --
procedure TBegaItem.loadShallow(Dataset: TDataset);
begin
  include(FState, biObjectLoading);
  try
    // load the data via doLoadShallow().
    // while item is in biLoading state, no change notifications are sent to owner
    inherited;
  finally
    exclude(FState, biObjectLoading);
  end;
  Dirty := FALSE;
  Loaded := TRUE;
end;

//- BG ----------------------------------------------------------- 14.05.2005 --
procedure TBegaItem.notifyOwner(ANotification: TBegaItemNotification);
begin
  if Owner <> nil then
    if (ANotification in [biDataChanged]) and (biObjectLoading in ItemState) then
      // surpress lots of biDataChanged notifications while loading.
      // 1 biObjectLoaded message at the end of loading is enough.
    else
      Owner.onItemNotification(self, ANotification);
end;

//- BG ----------------------------------------------------------- 14.05.2005 --
procedure TBegaItem.saveShallow(Dataset: TDataset);
begin
  inherited;
  // BG, 19.05.2005: hmm, if saving to database, there still might happen any shit until its really clean. Dirty := FALSE;
end;

//- BG ----------------------------------------------------------- 26.09.2005 --
procedure TBegaItem.setDirty(Value: Boolean);
begin
  if biObjectLoading in ItemState then
    // surpress lots of biDataChanged notifications while loading.
    // 1 biObjectLoaded message at the end of loading is enough.
  else if biDataChanging in ItemState then
    // surpress lots of biDataChanged notifications while updating.
    // 1 biDataChanged message at the end of updating is enough.
    if Value then
      include(FState, biDataChangePending)
    else
      exclude(FState, biDataChangePending)
  else
    setState(biDataChanged, Value)
end;

//- BG ----------------------------------------------------------- 14.05.2005 --
procedure TBegaItem.setOwner(const Value: TBegaItemOwner);
begin
  // setting (and thus notifying) the biOwnerChanging state
  // should cause an owning container to extract this item
  setState(biOwnerChanging, TRUE);
  FOwner := Value;
  setState(biOwnerChanging, FALSE);
end;

//- BG ----------------------------------------------------------- 13.05.2005 --
procedure TBegaItem.setState(State: TBegaItemState; Value: Boolean);
begin
  if Value then
  begin
    include(FState, State);
    notifyOwner(State); // always notify setting of state (biChanged is needed each time again)
  end
  else
    exclude(FState, State);
end;

{ TBegaItemOwner }

//- BG ----------------------------------------------------------- 14.05.2005 --
procedure TBegaItemOwner.onItemNotification(ASender: TBegaItem;
  ANotification: TBegaItemNotification);
begin
  {descendants should react on these notifications as follows:

  case ANotification of
    biObjectLoaded,
    biDataChanged:
      update(ASender);  // indexes need to update on any data changed
    biOwnerChanging:
      extract(ASender); // item moves to another owner or is destroyed
  end;

  }
end;

{ TBegaCustomKeyObject }

//- BG ----------------------------------------------------------- 10.05.2005 --
function TBegaCustomNameKeyObject.doCompare(WithObject: TBegaItem; Comparison, Fields: Integer): Integer;
begin
  case Comparison of
    CBegaIndexByKey: Result := Key - TBegaKeyObject(WithObject).Key;
    CBegaIndexByName: Result := WideCompareText(Name, TBegaKeyObject(WithObject).Name);
  else
    Result := inherited doCompare(WithObject, Comparison, Fields);
  end;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
class function TBegaCustomNameKeyObject.FieldCountOfComparison(Comparison: Integer): Integer;
begin
  case Comparison of
    CBegaIndexByKey, CBegaIndexByName: Result := 1;
  else
    Result := inherited FieldCountOfComparison(Comparison);
  end;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
class function TBegaCustomNameKeyObject.ComparisonCount: Integer;
begin
  Result := CBegaIndexByName + 1;
end;

//- BG ----------------------------------------------------------- 26.09.2005 --
class function TBegaCustomNameKeyObject.NewKey: TBegaKey;
const
  Key: TBegaKey = 0;
begin
  dec(Key);
  Result := Key;
end;

{ TBegaKeyObject }

//- BG ----------------------------------------------------------- 26.09.2005 --
constructor TBegaKeyObject.create;
begin
  inherited;
  FKey := NewKey;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
function TBegaKeyObject.getKey: Integer;
begin
  Result := FKey;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
function TBegaKeyObject.getName: TBegaName;
begin
  Result := '';
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
procedure TBegaKeyObject.setKey(const Key: Integer);
begin
  if FKey <> Key then
  begin
    FKey := Key;
    Dirty := TRUE;
  end;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
procedure TBegaKeyObject.setName(const Name: TBegaName);
begin
{$warn SYMBOL_PLATFORM off}
  raise EAbstractError.create(ClassName + '.setName() not implemented');
{$warn SYMBOL_PLATFORM on}
end;

{ TBegaNameObject }

//- BG ----------------------------------------------------------- 10.05.2005 --
function TBegaNameObject.getKey: Integer;
begin
  Result := 0;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
function TBegaNameObject.getName: WideString;
begin
  Result := FName;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
procedure TBegaNameObject.setKey(const Key: Integer);
begin
{$warn SYMBOL_PLATFORM off}
  raise EAbstractError.create(ClassName + '.setKey() not implemented');
{$warn SYMBOL_PLATFORM on}
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
procedure TBegaNameObject.setName(const Name: WideString);
begin
  if FName <> Name then
  begin
    FName := Name;
    Dirty := TRUE;
  end;
end;

{ TBegaNameKeyObject }

//- BG ----------------------------------------------------------- 26.09.2005 --
constructor TBegaNameKeyObject.create;
begin
  inherited;
end;

//- BG ----------------------------------------------------------- 26.09.2005 --
constructor TBegaNameKeyObject.createNew(NamePrefix: String);
begin
  inherited create;
  FKey := NewKey;
  FName := NamePrefix + intToStr(FKey);
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
function TBegaNameKeyObject.getKey: Integer;
begin
  Result := FKey;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
function TBegaNameKeyObject.getName: WideString;
begin
  Result := FName;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
procedure TBegaNameKeyObject.setKey(const Key: Integer);
begin
  if FKey <> Key then
  begin
    FKey := Key;
    Dirty := TRUE;
  end;
end;

//- BG ----------------------------------------------------------- 10.05.2005 --
procedure TBegaNameKeyObject.setName(const Name: WideString);
begin
  if FName <> Name then
  begin
    FName := Name;
    Dirty := TRUE;
  end;
end;

end.
