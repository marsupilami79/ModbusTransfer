unit ModbusTransfer;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ZDataset, SnapMB, ZDatasetParam, fgl, ZConnection, ZXmlCompat;

type
  TLogProcedure = procedure(Msg: String);

  TMbtInput = class
    protected
      FName: String;
      FParameter: TZParam;
    public
      Constructor Create(ZParam: TZParam);
      procedure SetAsInteger(NewValue: Integer);
      procedure SetAsDouble(NewValue: Double);
      procedure SetAsSingle(NewValue: Single);
      procedure SetAsWord(NewValue: Word);
    published
      property Name: String read FName write FName;
  end;

  TMbtInputList = class(TFPGList<TMbtInput>)
    function Find(InputName: String): TMbtInput;
  end;


  TMbtCustomAction = class
    protected
      FName: String;
      FInputList: TMbtInputList;
    public
      procedure Execute; virtual; abstract;
      property InputList: TMbtInputList read FInputList;
      constructor Create;
      destructor Destroy;
    published
      property Name: String read FName write FName;
  end;



  TMbtCustomOutput = class
    protected
      FName: String;
      FTargets: TMbtInputList;
    public
      property Targets: TMbtInputList read FTargets;
      constructor Create;
    published
  end;

  TMbtDataType = (dtWord, dtInteger, dtSingle, dtDouble);

  TMbtModbusOutput = class(TMbtCustomOutput)
    protected
      FOffset: Integer;
      FDataType: TMbtDataType;
      FInputList: TMbtInputList;
    public
      constructor Create;
      destructor Destroy; override;
      property InputList: TMbtInputList read FInputList;
    published
      property Offset: Integer read FOffset write FOffset;
      property DataType: TMbtDataType read FDataType write FDataType;
  end;

  TMbtModbusOutputList = class(TFPGList<TMbtModbusOutput>)

  end;

  TMbtSqlExecAction = class(TMbtCustomAction)
    protected
      FQuery: TZQuery;
      procedure SetSQL(NewSQL: String);
      function GetSQL: String;
    public
      procedure Execute; override;
      constructor Create(Connection: TZConnection);
      destructor Destroy;
    published
      property SQL: String read GetSQL write SetSQL;
  end;

  TMbtSqlSelectAction = class(TMbtCustomAction)
    protected
      FQuery: TZQuery;
      procedure SetSQL(NewSQL: String);
      function GetSQL: String;
    public
      procedure Execute; override;
      constructor Create(Connection: TZConnection);
      destructor Destroy;
    published
      property SQL: String read GetSQL write SetSQL;
  end;

  TMbtReadHoldingRegistersAction = class(TMbtCustomAction)
    protected
      FComPort: String;
      FBaudRate: Integer;
      FModbusFormat: TMBSerialFormat;
      FParity: Char;
      FDataBits: Integer;
      FStopBits: Integer;
      FFlowControl: TMBSerialFlow;
      FStartRegister: Word;
      FRegisterCount: Word;
      FDeviceAddress: Byte;
      FOutputList: TMbtModbusOutputList;
      procedure CheckError(Res: Integer);
    public
      procedure Execute; override;
      property OutputList: TMbtModbusOutputList read FOutputList;
      constructor Create;
      destructor Destroy;
      procedure CopyWord(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
      procedure CopyInt(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
      procedure CopySingle(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
      procedure CopyDouble(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
    published
      property ComPort:String read FComPort write FComPort;
      property BaudRate: Integer read FBaudRate write FBaudRate;
      property ModbusFormat: TMBSerialFormat read FModbusFormat write FModbusFormat;
      property Parity: Char read FParity write FParity;
      property DataBits: Integer read FDataBits write FDataBits;
      property StopBits: Integer read FStopBits write FStopBits;
      property FlowControl: TMBSerialFlow read FFlowControl write FFlowControl;
      property StartRegister: Word read FStartRegister write FStartRegister;
      property RegisterCount: Word read FRegisterCount write FRegisterCount;
      property DeviceAddress: Byte read FDeviceAddress write FDeviceAddress;
  end;

  TMbtActionGroup = class(TFPGList<TMbtCustomAction>)
    protected
      FName: String;
    public
      function FindAction(ActionName: String): TMbtCustomAction;
      property Name: String read FName write FName;
  end;

  TMbtGroupList = TFPGList<TMbtActionGroup>;

  TMbtConfig = class
    protected
      FConnection: TZConnection;
      FGroupList: TMbtGroupList;
      FLogProcedure: TLogProcedure;
      procedure LoadGroup(Node: IXMLNode);
      procedure LoadDbConfig(Node: IXMLNode);
      function LoadSqlExecAction(Node: IXMLNode): TMbtSqlExecAction;
      function LoadSqlSelectAction(Node: IXMLNode): TMbtSqlSelectAction;
      function LoadReadRegistersAction(Node: IXMLNode; ActionGroup: TMbtActionGroup): TMbtReadHoldingRegistersAction;
      procedure LoadModbusOutputs(Action: TMbtReadHoldingRegistersAction; OutputList: IXMLNodeList; ActionGroup: TMbtActionGroup);
      procedure LoadConnectedInputs(InputList: TMbtInputList; NodeList: IXMLNodeList; ActionGroup: TMbtActionGroup; const ActionName: String; const OutputNo: Integer);
      procedure Log(Msg: String);
    public
      procedure LoadFromFile(AFileName: String);
      procedure Execute;
      constructor Create;
      destructor Destroy;
      property LogProcedure: TLogProcedure read FLogProcedure write FLogProcedure;
  end;

implementation

uses
  variants;

function VarToInt(const AVar: Variant): Integer;
begin
  if VarIsNull(AVar) then Result := 0 else
    if VarIsOrdinal(AVar) then
      Result := AVar
    else
      Result := StrToIntDef(VarToStr(AVar), 0);
end;


constructor TMbtCustomAction.Create;
begin
  inherited;
  FInputList := TMbtInputList.Create;
end;

destructor TMbtCustomAction.Destroy;
begin
  if Assigned(FInputList) then
    FreeAndNil(FInputList);
  inherited;
end;


constructor TMbtCustomOutput.Create;
begin
  inherited;
  FTargets := TMbtInputList.Create;
end;





Constructor TMbtInput.Create(ZParam: TZParam);
begin
  inherited Create();
  FParameter := ZParam;
  FName := ZParam.Name;
end;

procedure TMbtInput.SetAsInteger(NewValue: Integer);
begin
  FParameter.AsInteger := NewValue;
end;

procedure TMbtInput.SetAsDouble(NewValue: Double);
begin
  FParameter.AsDouble := NewValue;
end;

procedure TMbtInput.SetAsSingle(NewValue: Single);
begin
  FParameter.AsSingle := NewValue;
end;

procedure TMbtInput.SetAsWord(NewValue: Word);
begin
  FParameter.AsWord := NewValue;
end;




constructor TMbtSqlExecAction.Create(Connection: TZConnection);
begin
  inherited Create;
  FQuery := TZQuery.Create(nil);
  FQuery.Connection := Connection;
end;

destructor TMbtSqlExecAction.Destroy;
begin
  if Assigned(FQuery) then
    FreeAndNil(FQuery);
  inherited;
end;

procedure TMbtSqlExecAction.SetSQL(NewSQL: String);
var
  x: Integer;
  Input: TMbtInput;
  ParamCount: Integer;
begin
  FQuery.SQL.Text := NewSQL;
  ParamCount := FQuery.Params.Count;
  FInputList.Clear;
  for x := 0 to ParamCount - 1 do begin
    Input := TMbtInput.Create(FQuery.Params[x]);
    FInputList.Add(Input);
  end;
end;

function TMbtSqlExecAction.GetSQL: String;
begin
  Result := FQuery.SQL.Text;
end;

procedure TMbtSqlExecAction.Execute;
begin
  FQuery.ExecSQL;
end;






constructor TMbtSqlSelectAction.Create(Connection: TZConnection);
begin
  inherited Create;
  FQuery := TZQuery.Create(nil);
  FQuery.Connection := Connection;
end;

destructor TMbtSqlSelectAction.Destroy;
begin
  if Assigned(FQuery) then
    FreeAndNil(FQuery);
end;

procedure TMbtSqlSelectAction.SetSQL(NewSQL: String);
begin
  FQuery.SQL.Text := NewSQL;
end;

function TMbtSqlSelectAction.GetSQL: String;
begin
  Result := FQuery.SQL.Text;
end;

procedure TMbtSqlSelectAction.Execute;
begin
  FQuery.Open;
  //Todo: Ergebnisse zuweisen
end;






constructor TMbtModbusOutput.Create;
begin
  inherited;
  FInputList := TMbtInputList.Create;
end;

destructor TMbtModbusOutput.Destroy;
begin
  if Assigned(FInputList) then
    FreeAndNil(FInputList);
end;






constructor TMbtReadHoldingRegistersAction.Create;
begin
  inherited;
  FOutputList := TMbtModbusOutputList.Create;
end;

destructor TMbtReadHoldingRegistersAction.Destroy;
begin
  if Assigned(FOutputList) then
    FreeAndNil(FOutputList);
  inherited;
end;

procedure TMbtReadHoldingRegistersAction.CheckError(Res: Integer);
begin
  if res <> 0 then
    raise Exception.Create('Modbus Error: $' + IntToHex(Res, 8));
end;

procedure TMbtReadHoldingRegistersAction.CopyWord(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
begin
  if Offset > High(Registers) then
    raise Exception.Create('Offset ' + IntToStr(Offset) + ' is out of scope for Words.') ;

  Input.SetAsWord(Registers[Offset]);
end;

procedure TMbtReadHoldingRegistersAction.CopyInt(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
begin
  if (Offset + (SizeOf(Integer) div 2) - 1) > High(Registers) then
    raise Exception.Create('Offset ' + IntToStr(Offset) + ' is out of scope for Integer.') ;

  Input.SetAsInteger(PInteger(@Registers[Offset])^);
end;

procedure TMbtReadHoldingRegistersAction.CopySingle(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
begin
  if (Offset + (SizeOf(Single) div 2) - 1) > High(Registers) then
    raise Exception.Create('Offset ' + IntToStr(Offset) + ' is out of scope for Single.') ;

  Input.SetAsSingle(PSingle(@Registers[Offset])^);
end;

procedure TMbtReadHoldingRegistersAction.CopyDouble(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
begin
  if (Offset + (SizeOf(Double) div 2) - 1) > High(Registers) then
    raise Exception.Create('Offset ' + IntToStr(Offset) + ' is out of scope for Double.') ;

  Input.SetAsSingle(PSingle(@Registers[Offset])^);
end;

procedure TMbtReadHoldingRegistersAction.Execute;
var
  Dev: TSnapMBBroker;
  Res: Integer;
  Registers: Array of Word;
  x, y: Integer;
begin
  SetLength(Registers, FRegisterCount);

  Dev := TSnapMBBroker.Create(FModbusFormat, FComPort, FBaudRate, FParity, FDataBits, FStopBits, FFlowControl);
  Dev.ChangeTo(FModbusFormat, FComPort, FBaudRate, FParity, FDataBits, FStopBits, FFlowControl);
  Dev.SetLocalParam(par_BaseAddressZero, 1);
  Dev.SetLocalParam(par_MaxRetries, 2);
  Res := Dev.Connect;
  CheckError(res);
  Res := Dev.ReadHoldingRegisters(FDeviceAddress, FStartRegister, FRegisterCount, @Registers[0]);
  CheckError(res);

  for x := 0 to FOutputList.Count - 1 do begin
    for y := 0 to FOutputList.Items[x].InputList.Count - 1 do begin;
      case FOutputList.Items[x].DataType of
        dtWord: CopyWord(Registers, FOutputList.Items[x].Offset, FOutputList.Items[x].InputList.Items[y]);
        dtInteger: CopyInt(Registers, FOutputList.Items[x].Offset, FOutputList.Items[x].InputList.Items[y]);
        dtSingle: CopySingle(Registers, FOutputList.Items[x].Offset, FOutputList.Items[x].InputList.Items[y]);
        dtDouble: CopyDouble(Registers, FOutputList.Items[x].Offset, FOutputList.Items[x].InputList.Items[y]);
      end;
    end;
  end;
end;



function TMbtActionGroup.FindAction(ActionName: String): TMbtCustomAction;
var
  x: Integer;
  ItemName: String;
begin
  ActionName := LowerCase(ActionName);
  Result := nil;
  for x := 0 to Count - 1 do begin
    ItemName := LowerCase(Items[x].Name);
    if ItemName = ActionName then begin;
      Result := Items[x];
      break;
    end;
  end;
end;





function TMbtInputList.Find(InputName: String): TMbtInput;
var
  x: Integer;
  ItemName: String;
begin
  InputName := LowerCase(InputName);
  Result := nil;
  for x := 0 to Count - 1 do begin
    ItemName := LowerCase(Items[x].Name);
    if ItemName = InputName then begin;
      Result := Items[x];
      break;
    end;
  end;
end;





constructor TMbtConfig.Create;
begin
  inherited;
  FConnection := TZConnection.Create(nil);
  FGroupList := TMbtGroupList.Create;
end;

destructor TMbtConfig.Destroy;
begin

  if Assigned(FConnection) then
    FreeAndNil(FConnection);
  inherited;
end;

procedure TMbtConfig.LoadFromFile(AFileName: String);
var
  ConfigFile: IXmlDocument;
  RootList: IXMLNodeList;
  GroupList: IXMLNodeList;
  x: Integer;
  RootNode: IXMLNode;
  DBNode: IXmlNode;
  GroupNode: IXMLNode;
  GroupName: String;
begin
  try
    ConfigFile := TZXmlDocument.Create as IXmlDocument;
    ConfigFile.LoadFromFile(AFileName);

    RootList := ConfigFile.GetChildNodes;
    if RootList.Count = 1 then begin
      RootNode := RootList.Get(0);
      if RootNode.GetNodeName <> 'mbtconfig' then
        raise Exception.Create('The configuration root node name must be "mbtconfig".');

      DBNode := RootNode.ChildNodes.FindNode('database');
      if not Assigned(DBNode) then
        raise Exception.Create('No database config node found!');

      LoadDbConfig(DBNode);

      GroupNode := RootNode.ChildNodes.FindNode('groups');
      if not Assigned(GroupNode) then
        raise Exception.Create('No group node found!');

      GroupList := GroupNode.ChildNodes;

      if GroupList.Count > 0 then begin
        for x := 0 to GroupList.Count - 1 do begin
          GroupNode := GroupList.Get(x);
          if GroupNode.GetNodeName <> 'group' then
            raise Exception.Create('No node types besides "group" are allowed inside the groups tag! Found: ' + GroupNode.GetNodeName);
          LoadGroup(GroupNode);
        end;
      end;
    end else if RootList.Count = 0 then
      raise Exception.Create('No XML Root List found!')
    else
      raise Exception.Create('More than one root node found. First node: ' + RootList.Get(0).GetNodeName);
  except
    on E: Exception do begin
      Log('Error while loading configuration: ' + E.Message);
    end;
  end;
end;

procedure TMbtConfig.LoadGroup(Node: IXMLNode);
var
  Group: TMbtActionGroup;
  GroupName: String;
  ActionList: IXMLNodeList;
  ActionNode: IXMLNode;
  x: Integer;
  ActionType: String;
  ActionName: String;
begin
  GroupName := VarToStr(Node.GetAttribute('name'));
  Group := TMbtActionGroup.Create;
  Group.Name := GroupName;

  ActionList := Node.GetChildNodes;
  for x := ActionList.Count - 1 downto 0 do begin
    ActionNode := ActionList.Get(x);
    if ActionNode.GetNodeName <> 'action' then
      raise Exception.Create('No node types besides "action" are allowed in the group node. Groupname: ' + GroupName + ' Node Index: ' + IntToStr(x));

    ActionName := VarToStr(ActionNode.GetAttribute('name'));
    if ActionName = '' then
      raise Exception.Create('Action with index ' + IntToStr(x) + ' has no name.');
    ActionType := VarToStr(ActionNode.GetAttribute('type'));
    if ActionType = 'readregisters' then begin
      Group.Insert(0, LoadReadRegistersAction(ActionNode, Group));
    end else if ActionType = 'sqlexec' then begin
      Group.Insert(0, LoadSqlExecAction(ActionNode));
    end else if ActionType = 'sqlselect' then begin
      Group.Insert(0, LoadSqlSelectAction(ActionNode));
    end else raise
      Exception.Create('Action "' + ActionName + '" has unsupported type "' + ActionType + '".');
  end;

  FGroupList.Add(Group);
end;

function TMbtConfig.LoadSqlExecAction(Node: IXMLNode): TMbtSqlExecAction;
var
  SQLNode: IXMLNode;
  x: Integer;
begin
  Result := TMbtSqlExecAction.Create(FConnection);
  Result.Name := VarToStr(Node.Attributes['name']);
  SQLNode := Node.GetChildNodes.FindNode('sql');
  if not Assigned(SQLNode) then
    raise Exception.Create('Action "' + Result.Name + '" has no sql node.');
  Result.SQL := SQLNode.GetText;
end;

function TMbtConfig.LoadSqlSelectAction(Node: IXMLNode): TMbtSqlSelectAction;
begin
  // ToDo: Implement LoadSqlSelectAction
  raise Exception.Create('Not supported yet!');
end;

function TMbtConfig.LoadReadRegistersAction(Node: IXMLNode; ActionGroup: TMbtActionGroup): TMbtReadHoldingRegistersAction;
var
  TempStr: String;
  OutputsNode: IXMLNode;
begin
  Result := TMbtReadHoldingRegistersAction.Create;
  Result.Name := VarToStr(Node.Attributes['name']);
  Result.ComPort := VarToStr(Node.Attributes['comport']);
  Result.BaudRate := VarToInt(Node.Attributes['baudrate']);
  Result.ModbusFormat := sfRTU;
  TempStr := VarToStr(Node.Attributes['parity']);
  case Length(TempStr) of
    0: Result.Parity := 'N';
    1: Result.Parity := TempStr[1];
    else
      raise Exception.Create('Parity must be only one character.');
  end;
  Result.DataBits := VarToInt(Node.Attributes['databits']);
  Result.StopBits := VarToInt(Node.Attributes['stopbits']);
  Result.FlowControl := flowNone;
  Result.StartRegister := VarToInt(Node.Attributes['startregister']);
  Result.RegisterCount := VarToInt(Node.Attributes['registercount']);
  Result.DeviceAddress := VarToInt(Node.Attributes['deviceaddress']);

  OutputsNode := Node.ChildNodes.FindNode('outputs');
  if Assigned(OutputsNode) then
    LoadModbusOutputs(Result, OutputsNode.GetChildNodes, ActionGroup);
end;

procedure TMbtConfig.LoadModbusOutputs(Action: TMbtReadHoldingRegistersAction; OutputList: IXMLNodeList; ActionGroup: TMbtActionGroup);
var
  x: Integer;
  OutputNode: IXMLNode;
  OutPut: TMbtModbusOutput;
  TempStr: String;
  InputsNode: IXMLNode;
begin
  for x := 0 to OutputList.Count - 1 do begin
    OutputNode := OutputList.Get(x);
    Output := TMbtModbusOutput.Create;
    TempStr := VarToStr(OutputNode.Attributes['type']);
    if tempstr = 'word' then
      OutPut.DataType := dtWord
    else if TempStr = 'integer' then
      OutPut.DataType := dtInteger
    else if TempStr = 'single' then
      OutPut.DataType := dtSingle
    else if TempStr = 'double' then
      OutPut.DataType := dtDouble
    else
      raise Exception.Create('Data type "' + TempStr + 'is not supported on Modbus input #' + IntToStr(x + 1) + ' in action "' + Action.Name + '".');
    OutPut.Offset := OutputNode.Attributes['offset'];

    InputsNode := OutputNode.ChildNodes.FindNode('connectedinputs');
    if Assigned(InputsNode) then;
      LoadConnectedInputs(OutPut.InputList, InputsNode.ChildNodes, ActionGroup, Action.Name, x + 1);

    Action.OutputList.Add(OutPut);
  end;
end;

procedure TMbtConfig.LoadConnectedInputs(InputList: TMbtInputList; NodeList: IXMLNodeList; ActionGroup: TMbtActionGroup; const ActionName: String; const OutputNo: Integer);
var
  x: Integer;
  TargetActionName, TargetInputName: String;
  InputNode: IXMLNode;
  Action: TMbtCustomAction;
  Input: TMbtInput;
begin
  for x := 0 to NodeList.Count - 1 do begin
    InputNode := NodeList.Get(x);
    TargetActionName := VarToStr(InputNode.Attributes['action']);
    TargetInputName := VarToStr(InputNode.Attributes['name']);

    Action := ActionGroup.FindAction(TargetActionName);
    if not Assigned(Action) then
      raise Exception.Create('No action named "' + TargetActionName + '" was found. Action: ' + ActionName + ' Output: ' +  IntToStr(OutputNo));

    Input := Action.InputList.Find(TargetInputName);
    if not Assigned(Input) then
      raise Exception.Create('No input named "' + TargetInputName + '" was found in action "' + TargetActionName + '". Action: ' + ActionName + ' Output: ' +  IntToStr(OutputNo));

    InputList.Add(Input);
  end;
end;

procedure TMbtConfig.LoadDbConfig(Node: IXMLNode);
var
  ConfigList: IXMLNodeList;

  function GetStr(Item: String): String;
  var
    Node: IXMLNode;
  begin
    node := ConfigList.FindNode(Item);
    if Assigned(node)
      then Result := Node.GetText
    else
      Result := '';
  end;

begin
  ConfigList := Node.ChildNodes;
  FConnection.Protocol := GetStr('protocol');
  FConnection.HostName := GetStr('hostname');
  FConnection.Database := GetStr('database');
  FConnection.User := GetStr('user');
  FConnection.Password := GetStr('password');
  FConnection.ClientCodepage := GetStr('characterset');
  FConnection.LibraryLocation := GetStr('librarylocation');
end;

procedure TMbtConfig.Log(Msg: String);
begin
  if Assigned(FLogProcedure) then
    FLogProcedure(FormatDateTime('YYYY-MM-DD HH:NN:SS', Now) + ' ' + Msg);
end;

procedure TMbtConfig.Execute;
var
  x, y: Integer;
  Group: TMbtActionGroup;
  Action: TMbtCustomAction;
begin
  try
    FConnection.Connect;
  except
    on E: Exception do
      Log('Error while connecting to database: ' + E.Message);
  end;

  if FConnection.Connected then begin
    for x := 0 to FGroupList.Count - 1 do begin
      Group := FGroupList.Items[x];
      Log('Executing Group ' + Group.Name);
      try
        for y := 0 to Group.Count - 1 do begin
          Action := Group.Items[y];
          Action.Execute;
        end;
      except
        on E: Exception do begin
          Log('Error while Executing group ' + Group.Name  + ' in action ' + Action.Name + ': ' + E.Message);
          Log('Aborting Group ' + Group.Name);
        end;
      end;
    end;
  end;
  Log('Execution finished.');
end;

end.

