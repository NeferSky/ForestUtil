unit ForestConsts;

interface

uses
  Classes;

const
  // Settings and params
  S_REG_KEY: AnsiString = 'Software\NeferSky\ForestUtil';
  S_FORM_LEFT: AnsiString = 'FormLeft';
  S_FORM_TOP: AnsiString = 'FormTop';
  S_FORM_HEIGHT: AnsiString = 'FormHeight';
  S_FORM_WIDTH: AnsiString = 'FormWidth';
  S_EDIT_LEFT: AnsiString = 'EditLeft';
  S_EDIT_TOP: AnsiString = 'EditTop';
  S_EDIT_HEIGHT: AnsiString = 'EditHeight';
  S_EDIT_WIDTH: AnsiString = 'EditWidth';
  S_COMPRESS_COLUMNS: AnsiString = 'CompressColumns';
  S_CONTINUE_ON_ERROR: AnsiString = 'ContinueOnError';

  S_INI_GUI: AnsiString = 'GUI';
  S_TRAY_ENABLED: AnsiString = 'TrayEnabled';

  S_INI_QUERIES: AnsiString = 'Queries';
  S_READING_INDEX: AnsiString = 'ReadingIndex';
  S_WRITING_INDEX: AnsiString = 'WritingIndex';
  S_ACTUAL_COUNT: AnsiString = 'ActualCount';
  S_MAX_QUERIES_COUNT: AnsiString = 'MaxQueriesCount';

  S_INI_TEMPLATES: AnsiString = 'Templates';
  S_SELECT_TEMPLATE: AnsiString = 'SelectTemplate';
  S_INSERT_TEMPLATE: AnsiString = 'InsertTemplate';
  S_UPDATE_TEMPLATE: AnsiString = 'UpdateTemplate';
  S_DELETE_TEMPLATE: AnsiString = 'DeleteTemplate';

  S_INI_DATA: AnsiString = 'Data';
  S_DB_DATABASE: AnsiString = 'DatabaseName';
  S_DB_SERVER: AnsiString = 'ServerName';
  S_DB_PORT: AnsiString = 'Port';
  S_DB_UID: AnsiString = 'Login';
  S_DB_PASSWORD: AnsiString = 'Password';

  // File open
  S_EXCEL_PROVIDER4: AnsiString = 'Provider=Microsoft.Jet.OLEDB.4.0';
  S_EXCEL_PROVIDER12: AnsiString = 'Provider=Microsoft.ACE.OLEDB.12.0';
  S_EXCEL_NO_HEADER: AnsiString = 'Header=False';
  S_EXCEL_PASSWORD: AnsiString = 'Password=""';
  S_EXCEL_DATASOURCE: AnsiString = 'Data Source=';
  S_EXCEL_EXCELFILE: AnsiString = 'Excel File=';
  S_EXCEL_USERID: AnsiString = 'User ID=Admin';
  S_EXCEL_MODE: AnsiString = 'Mode=Share Deny None';
  S_EXCEL_EXTENDED4: AnsiString = 'Extended Properties="Excel 8.0;HDR=No"';
  S_EXCEL_EXTENDED12: AnsiString =
    'Extended Properties="Excel 12.0 Xml;HDR=No"';
  S_EXCEL_JET_DB: AnsiString = 'Jet OLEDB:System database=""';
  S_EXCEL_JET_REG: AnsiString = 'Jet OLEDB:Registry Path=""';
  S_EXCEL_JET_PASS: AnsiString = 'Jet OLEDB:Database Password=""';
  S_EXCEL_JET_ENGINE: AnsiString = 'Jet OLEDB:Engine Type=37';
  S_EXCEL_JET_LOCK: AnsiString = 'Jet OLEDB:Database Locking Mode=0';
  S_EXCEL_JET_BULK_OPS: AnsiString = 'Jet OLEDB:Global Partial Bulk Ops=2';
  S_EXCEL_JET_BULK_TRN: AnsiString = 'Jet OLEDB:Global Bulk Transactions=1';
  S_EXCEL_JET_NEWPASS: AnsiString = 'Jet OLEDB:New Database Password=""';
  S_EXCEL_JET_CRTDB: AnsiString = 'Jet OLEDB:Create System Database=False';
  S_EXCEL_JET_ENC: AnsiString = 'Jet OLEDB:Encrypt Database=False';
  S_EXCEL_JET_COPY: AnsiString =
    'Jet OLEDB:Don''t Copy Locale on Compact=False';
  S_EXCEL_JET_REPL: AnsiString =
    'Jet OLEDB:Compact Without Replica Repair=False';
  S_EXCEL_JET_SFP: AnsiString = 'Jet OLEDB:SFP=False';
  S_EXCEL_JET_COMPLEX: AnsiString = 'Jet OLEDB:Support Complex Data=False';
  S_XLS_FORMAT: AnsiString = '%s;%s%s;%s';
  S_XLSX_FORMAT: AnsiString =
    '%s;%s;%s%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s';

  // Database open
  S_PG_PROVIDER: AnsiString = 'Provider=MSDASQL.1';
  S_PG_PASSWORD: AnsiString = 'Password=';
  S_PG_SECURITY_INFO: AnsiString = 'Persist Security Info=True';
  S_PG_USER_ID: AnsiString = 'User ID=';
  S_PG_EXTENDED_PROPS: AnsiString = 'Extended Properties="';
  S_PG_DRIVER: AnsiString = 'DRIVER={PostgreSQL ANSI}';
  S_PG_DATABASE: AnsiString = 'DATABASE=';
  S_PG_SERVER: AnsiString = 'SERVER=';
  S_PG_PORT: AnsiString = 'PORT=';
  S_PG_UID: AnsiString = 'UID=';
  S_PG_PWD: AnsiString = 'PWD=';
  S_PG_SSL_MODE: AnsiString = 'SSLmode=disable';
  S_PG_READ_ONLY: AnsiString = 'ReadOnly=0';
  S_PG_PROTOCOL: AnsiString = 'Protocol=7.4';
  S_PG_FAKE_OID_INDEX: AnsiString = 'FakeOidIndex=0';
  S_PG_SHOW_OID_COLS: AnsiString = 'ShowOidColumn=0';
  S_PG_ROW_VERSIONONG: AnsiString = 'RowVersioning=0';
  S_PG_SHOW_SYS_TABLES: AnsiString = 'ShowSystemTables=0';
  S_PG_CONN_SETTINGS: AnsiString = 'ConnSettings=';
  S_PG_FETCH: AnsiString = 'Fetch=100';
  S_PG_SOCKET: AnsiString = 'Socket=4096';
  S_PG_UNKNOWN_SIZES: AnsiString = 'UnknownSizes=0';
  S_PG_VARCHAR_SIZE: AnsiString = 'MaxVarcharSize=255';
  S_PG_LVARCHAR_SIZE: AnsiString = 'MaxLongVarcharSize=8190';
  S_PG_DEBUG: AnsiString = 'Debug=0';
  S_PG_COMM_LOG: AnsiString = 'CommLog=0';
  S_PG_OPTIMIZER: AnsiString = 'Optimizer=0';
  S_PG_KSQO: AnsiString = 'Ksqo=1';
  S_PG_DECLARE_FETCH: AnsiString = 'UseDeclareFetch=0';
  S_PG_TEXT_LVARCHAR: AnsiString = 'TextAsLongVarchar=1';
  S_PG_UNKNOWNS_LVARCHAR: AnsiString = 'UnknownsAsLongVarchar=0';
  S_PG_BOOLS_AS_CHAR: AnsiString = 'BoolsAsChar=1';
  S_PG_PARSE: AnsiString = 'Parse=0';
  S_PG_CANCEL_FREE_STMT: AnsiString = 'CancelAsFreeStmt=0';
  S_PG_SYS_TABLE_PREFIXES: AnsiString = 'ExtraSysTablePrefixes=dd_;';
  S_PG_LF_CONVERSION: AnsiString = 'LFConversion=1';
  S_PG_UPD_CURSORS: AnsiString = 'UpdatableCursors=1';
  S_PG_DISALLOW_PREMATURE: AnsiString = 'DisallowPremature=0';
  S_PG_TRUE_IS_MINUS1: AnsiString = 'TrueIsMinus1=0';
  S_PG_BI: AnsiString = 'BI=0';
  S_PG_BYTEA_LVARBINARY: AnsiString = 'ByteaAsLongVarBinary=0';
  S_PG_SERVERSIDE_PREPARE: AnsiString = 'UseServerSidePrepare=1';
  S_PG_LOWERCASE_ID: AnsiString = 'LowerCaseIdentifier=0';
  S_PG_GSSAUTH_USE_GSS: AnsiString = 'GssAuthUseGSS=0';
  S_PG_XA_OPT: AnsiString = 'XaOpt=1"';
  S_PG_INIT_CATALOG: AnsiString = 'Initial Catalog=';
  S_POSTGRESQL_FORMAT: AnsiString =
    '%s;%s%s;%s;%s%s;%s%s;%s%s;%s%s;%s%s;%s%s;%s%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s%s';

  // User messages
  S_QUERY_TYPE_CONFIRM: AnsiString = 'Похоже, что выбран неверный тип запроса.'
    + #13#10 + 'Верно ли указан тип?';
  S_STATUS_WAITING_TABLE: AnsiString = 'Ожидание выбора таблицы';
  S_STATUS_POSITION_TABLE: AnsiString = 'Позиционирование таблицы';
  S_STATUS_READY: AnsiString = 'Готово к работе';
  S_STATUS_PROCESSING: AnsiString = 'Выполняется:';
  S_STATUS_OFFLINE: AnsiString = 'Отключено';
  S_LOG_ONE_MORE_THAN_TWO: AnsiString = '' + #13#10 +
    'Строка %d: Значение в колонке %d больше значения в колонке %d';
  S_LOG_SUM_ONE_DIFF_THAN_TWO: AnsiString = '' + #13#10 +
      'Строка %d: Сумма значений в колонках %s не равна значению в колонке %d';
  S_LOG_ONE_DIFF_THAN_SUM_TWO: AnsiString = '' + #13#10 +
      'Строка %d: Значение в колонке %d больше суммы значений в колонках %s';
  S_LOG_FORMULA_ONE_DIFF_THAN_TWO: AnsiString = '' + #13#10 +
      'Строка %d: Значения в колонках (%s) не равно значению в колонке %d';
  S_CONFIRM_REPLACE: AnsiString = 'Изменить "%s" на "%s"? %s';
  S_LOG_EMPTY_ROW: AnsiString = '' + #13#10 + 'Пустая строка: ';
  S_LOG_DUPLICATE_ROW: AnsiString = 'Дублирование строк: %d и %d';
  S_LOG_REPLACE_FROM_DICTIONARY: AnsiString =
    '' + #13#10 + 'Строка %d: значение "%s" из словаря вместо "%s"';
  S_LOG_COMPLETED: AnsiString = 'Завершено';

  // Error messages
  E_FIND_FIRST_CELL: AnsiString = 'Не удалось распознать первую строку таблицы';
  E_FIND_LAST_CELL: AnsiString =
    'Не удалось распознать последнюю строку таблицы';
  E_WRITE_QUERY: AnsiString = 'Ошибка при попытке запомнить SQL-запрос' +
    #13#10 + 'Значения счетчиков:' + #13#10 +
    'MaxQueriesCount: %d, FActualCount: %d, FWritingIndex: %d, FReadingIndex: %d';
  E_READ_QUERY: AnsiString = 'Ошибка при попытке прочитать SQL-запрос' +
    #13#10 + 'Значения счетчиков:' + #13#10 +
    'MaxQueriesCount: %d, FActualCount: %d, FWritingIndex: %d, FReadingIndex: %d';

  // File names
  S_DICTIONARY_VALID_PREFIX: AnsiString = 'Valid';
  S_QUERY_FILE_NAME: AnsiString = 'SQLQueries.lst';
  S_SETTINGS_FILE_NAME: AnsiString = 'Settings.ini';
  S_LOG_FILE_NAME: AnsiString = 'Validate.log';
  S_DICTIONARY_FORESTRIES_FILE: AnsiString = 'DictionaryForestry.dic';
  S_DICTIONARY_LOCAL_FORESTRIES_FILE: AnsiString =
    'DictionaryLocalForestry.dic';
  S_DICTIONARY_LANDUSE_FILE: AnsiString = 'DictionaryLanduse.dic';
  S_DICTIONARY_SPECIES_FILE: AnsiString = 'DictionarySpecies.dic';
  S_DICTIONARY_DAMAGE_FILE: AnsiString = 'DictionaryDamage.dic';
  S_DICTIONARY_PEST_FILE: AnsiString = 'DictionaryPest.dic';

  // Dictionary names
  S_DICTIONARY_FORESTRIES_NAME: AnsiString = 'Лесничество';
  S_DICTIONARY_LOCAL_FORESTRIES_NAME: AnsiString = 'Участковое лесничество';
  S_DICTIONARY_LANDUSE_NAME: AnsiString = 'Целевое назначение лесов';
  S_DICTIONARY_SPECIES_NAME: AnsiString = 'Порода';
  S_DICTIONARY_DAMAGE_NAME: AnsiString =
    'Основная причина ослабления (усыхания)';
  S_DICTIONARY_PEST_NAME: AnsiString = 'Вид вредного организма';

  // Settings names
  S_SETTINGS_TEMPLATE_SELECT: AnsiString = 'Шаблон SELECT';
  S_SETTINGS_TEMPLATE_INSERT: AnsiString = 'Шаблон INSERT';
  S_SETTINGS_TEMPLATE_UPDATE: AnsiString = 'Шаблон UPDATE';
  S_SETTINGS_TEMPLATE_DELETE: AnsiString = 'Шаблон DELETE';
  S_SETTINGS_DB_DATABASE: AnsiString = 'Имя базы данных';
  S_SETTINGS_DB_SERVER: AnsiString = 'Имя сервера';
  S_SETTINGS_DB_PORT: AnsiString = 'Порт';
  S_SETTINGS_DB_LOGIN: AnsiString = 'Имя пользователя';
  S_SETTINGS_DB_PASSWORD: AnsiString = 'Пароль БД';
  S_SETTINGS_QUERIES_REMEMBERED: AnsiString = 'Запоминать запросов';
  S_SETTINGS_HIDE_TO_TRAY: AnsiString = 'Сворачиваться в трей';
  S_VERSION: AnsiString = 'Версия: %s';
  S_COPYRIGHT: AnsiString = 'Axl NeferSky (AxlNeferSky@gmail.com)';
  S_COMMENTS: AnsiString = 'Комментарии излишни';

  // Numbers
  I_FILE_INDEX = $FFFFFFFF;
  I_DEFAULT_COL_WIDTH = 64;
  I_COL_COUNT = 68;
  I_MIN_HEIGHT = 50;
  I_NORMAL_HEIGHT = 300;
  I_COLS_TO_FIND_LAST_ROW = 11;
  I_WEIGHT_TO_FIND_LAST_ROW = 6;

  // Script
  S_DB_TABLE_NAME: AnsiString = 'QuarterReports';
  S_DB_SCRIPT_HEADER: AnsiString = '';
  S_DB_SCRIPT_FOOTER: AnsiString = 'COMMIT;';
  S_DB_SCRIPT_LN_HEADER: AnsiString = '';
  S_DB_SCRIPT_LN_FOOTER: AnsiString = '';

  S_DB_INSERT_SCRIPT_FORMAT_BEGIN: AnsiString = 'INSERT INTO %s (';
  S_DB_INSERT_SCRIPT_FORMAT_MIDDLE: AnsiString = ') VALUES (';
  S_DB_INSERT_SCRIPT_FORMAT_END: AnsiString = ');';

  S_DB_INSERT_SCRIPT_FORMAT_FLD1: AnsiString =
    'id, input_date, forestry_number, local_forestry_number, quarter, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD2: AnsiString =
    'land, landuse_purpose_code, defense_category, species_id, damaged_species_id, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD3: AnsiString =
    'cause_code, damage_year, damage_cause_group, land_area, examined_all, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD4: AnsiString =
    'examined_by_RLZ, examined_by_forestries, examined_by_contractor, damaged_year_begin, damaged_found, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD5: AnsiString =
    'damaged_period_end, damaged_less_4, damaged_4_to_10, damaged_10_to_40, damaged_more_40, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD6: AnsiString =
    'lost_period_begin, lost_year_begin, survive_period_end, planned_events_by_SSR, planned_events_by_VSR, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD7: AnsiString =
    'planned_events_by_UZ, planned_event_type, planned_event_area, done_events_SSR_period_total, done_events_SSR_period_total_rent, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD8: AnsiString =
    'done_events_SSR_year_total, done_events_SSR_year_total_rent, done_events_SSR_supply_total, done_events_SSR_supply_total_rent, done_events_VSR_period_total, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD9: AnsiString =
    'done_events_VSR_period_total_rent, done_events_VSR_year_total, done_events_VSR_year_total_rent, done_events_VSR_supply_total, done_events_VSR_supply_total_rent,';
  S_DB_INSERT_SCRIPT_FORMAT_FLD10: AnsiString =
    'done_events_UZ_period_total, done_events_UZ_period_total_rent, done_events_UZ_year_total, done_events_UZ_year_total_rent, done_events_UZ_supply_total, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD11: AnsiString =
    'done_events_UZ_supply_total_rent, done_other_event_type, done_other_events_period_total, done_other_event_period_rent, done_other_events_year_total, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD12: AnsiString =
    'done_other_event_year_rent, done_other_events_supply_total, done_other_event_supply_rent, damage_state, pest_code, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD13: AnsiString =
    'damage_area_year_begin, damage_recedive, damage_liquidated, damage_self_extinguished, damage_period_total, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD14: AnsiString =
    'damage_require_period_end, damage_strength_low, damage_strength_middle, damage_strength_high, damage_strength_full';

  S_DB_INSERT_SCRIPT_FORMAT_VAL1: AnsiString =
    '''%s'', ''%s'', %d, %d, %d, %d, %d, ''%s'', %d, %d, ';
  S_DB_INSERT_SCRIPT_FORMAT_VAL2: AnsiString =
    '%d, %d, %d, %s, %s, %s, %s, %s, %s, %s, ';
  S_DB_INSERT_SCRIPT_FORMAT_VAL3: AnsiString =
    '%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, ';
  S_DB_INSERT_SCRIPT_FORMAT_VAL4: AnsiString =
    '%s, ''%s'', %s, %s, %s, %s, %s, %s, %s, %s, ';
  S_DB_INSERT_SCRIPT_FORMAT_VAL5: AnsiString =
    '%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, ';
  S_DB_INSERT_SCRIPT_FORMAT_VAL6: AnsiString =
    '%s, ''%s'', %s, %s, %s, %s, %s, %s, ''%s'', %d, ';
  S_DB_INSERT_SCRIPT_FORMAT_VAL7: AnsiString =
    '%s, %s, %s, %s, %s, %s, %s, %s, %s, %s';

  S_DB_GET_REGION_ID_BY_FORESTRY: AnsiString =
    'select region_id from forestries where upper(forestry_name) like ''%s''';
  S_DB_GET_FORESTRY_ID: AnsiString =
    'select forestry_id from forestries where upper(forestry_name) like ''%s''';
  S_DB_GET_LOCAL_FORESTRY_ID: AnsiString =
    'select local_forestry_id from local_forestries where upper(local_forestry_name) like ''%s''';
  S_DB_GET_PEST_CODE: AnsiString =
    'select pest_code from pest_dict where upper(pest_rus) like ''%s''';
  S_DB_GET_LANDUSE_PURPOSE_CODE: AnsiString =
    'select landuse_purpose_code from landuse_purposes where upper(landuse_purpose) like ''%s''';
  S_DB_GET_SPECIES_ID: AnsiString =
    'select species_id from species where upper(species_name_russian) like ''%s''';
  S_DB_GET_CAUSE_CODE: AnsiString =
    'select cause_code from damage_causes where upper(cause_rus) like ''%s''';
  S_DB_GET_CURRENT_MOUNTH: AnsiString =
    'select date_part(''month'', current_timestamp)';
  S_DB_GET_CURRENT_YEAR: AnsiString =
    'select date_part(''year'', current_timestamp)';

  S_SQL_GET_FORESTRIES_DICT: AnsiString =
    'select distinct forestry_name from forestries where region_id in (22, 23) order by forestry_name';
  S_SQL_GET_LOCAL_FORESTRIES_DICT: AnsiString =
    'select distinct local_forestry_name from local_forestries where region_id in (22, 23) order by local_forestry_name';
  S_SQL_GET_LOCAL_FORESTRIES_BY_FORESTRY: AnsiString =
    'select distinct local_forestry_name from local_forestries where forestry_id = (%d) order by local_forestry_name';
  S_SQL_GET_LANDUSE_DICT: AnsiString =
    'select distinct landuse_purpose from landuse_purposes order by landuse_purpose';
  S_SQL_GET_SPECIES_DICT: AnsiString =
    'select distinct poroda from species order by poroda';
  S_SQL_GET_DAMAGE_DICT: AnsiString =
    'select distinct cause_rus_short from damage_causes order by cause_rus';
  S_SQL_GET_PEST_DICT: AnsiString =
    'select distinct pest_rus_short from pest_dict order by pest_rus';

  // =)
  S_YES = 'True';
  S_NO = 'False';
  S_FREE_SELECT: AnsiString = 'SELECT';
  S_DOTCOMMA_SELECT: AnsiString = ';SELECT';
  S_BRACKED1_SELECT: AnsiString = '(SELECT';
  S_BRACKED2_SELECT: AnsiString = ')SELECT';
  S_EQUAL_SELECT: AnsiString = '=SELECT';
  S_FILE_SELECT: AnsiString = 'select * from [%s]';

var
  SQLSelectTemplate: AnsiString = ' SELECT * FROM @ WHERE @ ORDER BY @ ;';
  SQLInsertTemplate: AnsiString = ' INSERT INTO @ VALUES @; ';
  SQLUpdateTemplate: AnsiString = ' UPDATE @ SET @ WHERE @; ';
  SQLDeleteTemplate: AnsiString = ' DELETE FROM @ WHERE @; ';

  PgDatabase: AnsiString = 'DatabaseName';
  PgServer: AnsiString = '127.0.0.1';
  PgPort: AnsiString = '5432';
  PgUID: AnsiString = 'postgres';
  PgPassword: AnsiString = '********';

  TrayEnabled: Boolean = False;
  MaxQueriesCount: Integer = 50;

  C_LIST_YESNO: TStringList;

implementation

//---------------------------------------------------------------------------

procedure CreateLists;
begin
  C_LIST_YESNO := TStringList.Create;
  C_LIST_YESNO.Clear;
  C_LIST_YESNO.Add(S_NO);
  C_LIST_YESNO.Add(S_YES);
end;

//---------------------------------------------------------------------------

procedure DestroyLists;
begin
  C_LIST_YESNO.Clear;
  C_LIST_YESNO.Free;
end;

//---------------------------------------------------------------------------

initialization
  CreateLists();

finalization
  DestroyLists();

end.
 
