unit ModbusTransfer;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ZDataset, SnapMB, ZDatasetParam, fgl, ZConnection;

type
  TMbtCustomAction = class
    protected
      FName: String;
    public
      procedure Execute; virtual; abstract;
    published
      property Name: String read FName write FName;
  end;

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
      FInputList: TMbtInputList;
      procedure SetSQL(NewSQL: String);
      function GetSQL: String;
    public
      procedure Execute; override;
      constructor Create(Connection: TZConnection);
      destructor Destroy; override;
      property InputList: TMbtInputList read FInputList;
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
      destructor Destroy; override;
    published
      property SQL: String read GetSQL write SetSQL;
  end;

  TMbtModbusAction = class(TMbtCustomAction)
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
      destructor Destroy; override;
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

implementation

constructor TMbtCustomOutput.Create;
begin
  inherited;
  FTargets := TMbtInputList.Create;
end;





Constructor TMbtInput.Create(ZParam: TZParam);
begin
  inherited Create();
  FParameter := ZParam;
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
  FInputList := TMbtInputList.Create;
end;

destructor TMbtSqlExecAction.Destroy;
begin
  if Assigned(FInputList) then
    FreeAndNil(FInputList);
  if Assigned(FQuery) then
    FreeAndNil(FQuery);
  inherited;
end;

procedure TMbtSqlExecAction.SetSQL(NewSQL: String);
var
  x: Integer;
begin
  FQuery.SQL.Text := NewSQL;
  FInputList.Clear;
  for x := 0 to FQuery.Params.Count - 1 do begin
    FInputList.Add(TMbtInput.Create(FQuery.Params[x]));
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






constructor TMbtModbusAction.Create;
begin
  inherited;
  FOutputList := TMbtModbusOutputList.Create;
end;

destructor TMbtModbusAction.Destroy;
begin
  if Assigned(FOutputList) then
    FreeAndNil(FOutputList);
  inherited;
end;

procedure TMbtModbusAction.CheckError(Res: Integer);
begin
  if res <> 0 then
    raise Exception.Create('Error: $' + IntToHex(Res, 8));
end;

procedure TMbtModbusAction.CopyWord(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
begin
  if Offset > High(Registers) then
    raise Exception.Create('Offset ' + IntToStr(Offset) + ' is out of scope for Words.') ;

  Input.SetAsWord(Registers[Offset]);
end;

procedure TMbtModbusAction.CopyInt(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
begin
  if (Offset + (SizeOf(Integer) div 2) - 1) > High(Registers) then
    raise Exception.Create('Offset ' + IntToStr(Offset) + ' is out of scope for Integer.') ;

  Input.SetAsInteger(PInteger(@Registers[Offset])^);
end;

procedure TMbtModbusAction.CopySingle(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
begin
  if (Offset + (SizeOf(Single) div 2) - 1) > High(Registers) then
    raise Exception.Create('Offset ' + IntToStr(Offset) + ' is out of scope for Single.') ;

  Input.SetAsSingle(PSingle(@Registers[Offset])^);
end;

procedure TMbtModbusAction.CopyDouble(const Registers: Array of Word; Offset: Integer; Input: TMbtInput);
begin
  if (Offset + (SizeOf(Double) div 2) - 1) > High(Registers) then
    raise Exception.Create('Offset ' + IntToStr(Offset) + ' is out of scope for Double.') ;

  Input.SetAsSingle(PSingle(@Registers[Offset])^);
end;

procedure TMbtModbusAction.Execute;
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

end.

