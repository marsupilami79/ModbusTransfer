unit ModbusTransfer;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ZDataset, SnapMB, ZDatasetParam, fgl;

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
    published
      property Offset: Integer read FOffset write FOffset;
      property DataType: TMbtDataType read FDataType write FDataType;
  end;

  TMbtSqlExecAction = class(TMbtCustomAction)
    protected
      Query: TZQuery;
      procedure SetSQL(NewSQL: String);
      function GetSQL: String;
    public
      procedure Execute; override;
    published
      property SQL: String read GetSQL write SetSQL;
  end;

  TMbtSqlSelectAction = class(TMbtCustomAction)
    protected
      Query: TZQuery;
      procedure SetSQL(NewSQL: String);
      function GetSQL: String;
    public
      procedure Execute; override;
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
      procedure CheckError(Res: Integer);
    public
      procedure Execute; override;
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






procedure TMbtSqlExecAction.SetSQL(NewSQL: String);
begin
  Query.SQL.Text := NewSQL;
end;

function TMbtSqlExecAction.GetSQL: String;
begin
  Result := Query.SQL.Text;
end;

procedure TMbtSqlExecAction.Execute;
begin
  Query.ExecSQL;
end;






procedure TMbtSqlSelectAction.SetSQL(NewSQL: String);
begin
  Query.SQL.Text := NewSQL;
end;

function TMbtSqlSelectAction.GetSQL: String;
begin
  Result := Query.SQL.Text;
end;

procedure TMbtSqlSelectAction.Execute;
begin
  Query.Open;
  //Todo: Ergebnisse zuweisen
end;



procedure TMbtModbusAction.CheckError(Res: Integer);
begin
  if res <> 0 then
    raise Exception.Create('Error: $' + IntToHex(Res, 8));
end;

procedure TMbtModbusAction.Execute;
var
  Dev: TSnapMBBroker;
  Res: Integer;
  Registers: Array of Word;
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

  // Todo: Copy Data to registers
end;

end.

