unit ForestConsts;

interface

uses
  Classes;

const
  // Numbers
  I_FILE_INDEX = $FFFFFFFF;
  I_DEFAULT_COL_WIDTH = 64;
  I_COL_COUNT = 68;
  I_MIN_HEIGHT = 50;
  I_NORMAL_HEIGHT = 300;
  I_COLS_TO_FIND_LAST_ROW = 11;
  I_WEIGHT_TO_FIND_LAST_ROW = 6;

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
  S_MATH_ERRORS: AnsiString = 'LogMathErrors';
  S_DUPLICATES: AnsiString = 'LogDuplicates';
  S_RELATION_ERRORS: AnsiString = 'LogRelationErrors';
  S_DICT_REPLACES: AnsiString = 'LogDictReplaces';
  S_EMPTY_RECORDS: AnsiString = 'LogEmptyRecords';
  S_PREV_REPORT_SUM: AnsiString = 'LogPrevReportSum';

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

  S_INI_DICT_FORMATS: AnsiString = 'DictionaryFormats';

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
  S_EXCEL_EXTENDED12: AnsiString = 'Extended Properties="Excel 12.0 Xml;HDR=No"';
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
  S_EXCEL_JET_COPY: AnsiString = 'Jet OLEDB:Don''t Copy Locale on Compact=False';
  S_EXCEL_JET_REPL: AnsiString = 'Jet OLEDB:Compact Without Replica Repair=False';
  S_EXCEL_JET_SFP: AnsiString = 'Jet OLEDB:SFP=False';
  S_EXCEL_JET_COMPLEX: AnsiString = 'Jet OLEDB:Support Complex Data=False';
  S_XLS_FORMAT: AnsiString = '%s;%s%s;%s';
  S_XLSX_FORMAT: AnsiString = '%s;%s;%s%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s';

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
  S_POSTGRESQL_FORMAT: AnsiString = '%s;%s%s;%s;%s%s;%s%s;%s%s;%s%s;%s%s;%s%s;%s%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s%s';

  // User messages
  S_QUERY_TYPE_CONFIRM: AnsiString = 'Похоже, что выбран неверный тип запроса.' + sLineBreak + 'Верно ли указан тип?';
  S_STATUS_WAITING_TABLE: AnsiString = 'Ожидание выбора таблицы';
  S_STATUS_POSITION_TABLE: AnsiString = 'Позиционирование таблицы';
  S_STATUS_READY: AnsiString = 'Готово к работе';
  S_STATUS_PROCESSING: AnsiString = 'Выполняется:';
  S_STATUS_OFFLINE: AnsiString = 'Отключено';

  S_LOG_ONE_MORE_THAN_TWO: AnsiString = '' + sLineBreak + 'Строка %d: Значение в колонке %d больше значения в колонке %d';
  S_LOG_SUM_ONE_DIFF_THAN_TWO: AnsiString = '' + sLineBreak + 'Строка %d: Сумма значений в колонках %s не равна значению в колонке %d';
  S_LOG_ONE_DIFF_THAN_SUM_TWO: AnsiString = '' + sLineBreak + 'Строка %d: Значение в колонке %d больше суммы значений в колонках %s';
  S_LOG_FORMULA_ONE_DIFF_THAN_TWO: AnsiString = '' + sLineBreak + 'Строка %d: Значения в колонках (%s) не равно значению в колонке %d';
  S_LOG_EMPTY_ROW: AnsiString = '' + sLineBreak + 'Пустая строка: %d';
  S_LOG_DUPLICATE_ROW: AnsiString = '' + sLineBreak + 'Дублирование строк: %d и %d';
  S_LOG_REPLACE_FROM_DICTIONARY: AnsiString = '' + sLineBreak + 'Строка %d: значение "%s" из словаря вместо "%s"';
  S_LOG_NO_SPECIES_RELATION: AnsiString = '' + sLineBreak + 'Строка %d: Повреждаемая порода не соответствует причине повреждения';
  S_LOG_NO_CAUSE_RELATION: AnsiString = '' + sLineBreak + 'Строка %d: Причина повреждения не соответствует году повреждения';
  S_LOG_INVALID_SUM_PREV_REPORT: AnsiString = '' + sLineBreak + 'Сумма значений в колонке %d не сходится с суммой из предыдущего отчета';
  S_LOG_PREV_REPORT_NOT_FOUND: AnsiString = '' + sLineBreak + 'Предыдущий отчет для сравнения не найден';

  S_LOG_COMPLETED: AnsiString = 'Завершено';
  S_LOG_FORCE_STOP: AnsiString = 'Экстренное завершение по требованию';
  S_LOG_SUCCESSFULLY: AnsiString = 'Проверка пройдена успешно - файл может быть загружен в БД';
  S_LOG_EXTRA_INVALID: AnsiString = 'Площадь очага больше площади обследования (см. номера строк и колонок в результатах проверки)';
  S_LOG_MAIN_INVALID: AnsiString = 'В таблице присутствуют критические ошибки - необходима проверка или доработка (см. номера строк и колонок в результатах проверки)';
  S_LOG_DUPLICATE_INVALID: AnsiString = 'В таблице присутствуют дубликаты строк, необходима проверка';
  S_LOG_RELATION_INVALID: AnsiString = 'В таблице присутствуют ошибки соответствия полей';
  S_LOG_SKIPPED_LINES: AnsiString = 'Обнаружены пропущенные проверки. Поэтому скрипт для базы данных я Вам не отдам...';

  S_IN_PROGRESS: AnsiString = 'Похоже, все еще выполняется предыдущий запрос.';
  S_QUERY_EXEC_SUCCESS: AnsiString = 'Успешно!';
  S_EDIT_PROMPT: AnsiString = '' + sLineBreak + sLineBreak + 'Значения близких по смыслу колонок:' + sLineBreak;

  // Error messages
  E_FIND_FIRST_CELL: AnsiString = 'Не удалось распознать первую строку таблицы';
  E_FIND_LAST_CELL: AnsiString = 'Не удалось распознать последнюю строку таблицы';
  E_WRITE_QUERY: AnsiString = 'Ошибка при попытке запомнить SQL-запрос' + sLineBreak +
    'Значения счетчиков:' + sLineBreak +
    'MaxQueriesCount: %d, FActualCount: %d, FWritingIndex: %d, FReadingIndex: %d';
  E_READ_QUERY: AnsiString = 'Ошибка при попытке прочитать SQL-запрос' + sLineBreak +
    'Значения счетчиков:' + sLineBreak +
    'MaxQueriesCount: %d, FActualCount: %d, FWritingIndex: %d, FReadingIndex: %d';
  E_QUERY_EXEC_ERROR: AnsiString = '!!! Ошибка при выполнении запроса. !!!' + sLineBreak +
    'Изменения отменены.';
  E_REPORT_ERROR = 'Во время подготовки отчета произошла ошибка.';

  // File names
  S_DICT_VALID_PREFIX: AnsiString = 'Valid';
  S_QUERY_FILE_NAME: AnsiString = 'SQLQueries.lst';
  S_SETTINGS_FILE_NAME: AnsiString = 'Settings.ini';
  S_LOG_FILE_NAME: AnsiString = 'Validate.log';

  // Dict files
  S_DICT_FORESTRIES_FILE: AnsiString = 'DictionaryForestry.dic';
  S_DICT_LOCAL_FORESTRIES_FILE: AnsiString = 'DictionaryLocalForestry.dic';
  S_DICT_LANDUSE_FILE: AnsiString = 'DictionaryLanduse.dic';
  S_DICT_PROTECT_CATEGORY_FILE: AnsiString = 'DictionaryProtectCategory.dic';
  S_DICT_SPECIES_FILE: AnsiString = 'DictionarySpecies.dic';
  S_DICT_DAMAGE_FILE: AnsiString = 'DictionaryDamage.dic';
  S_DICT_PEST_FILE: AnsiString = 'DictionaryPest.dic';

  // Dictionary captions
  S_DICT_FORESTRIES_NAME: AnsiString = 'Лесничество';
  S_DICT_LOCAL_FORESTRIES_NAME: AnsiString = 'Участковое лесничество';
  S_DICT_LANDUSE_NAME: AnsiString = 'Целевое назначение лесов';
  S_DICT_PROTECT_CATEGORY_NAME: AnsiString = 'Категория защитных лесов';
  S_DICT_SPECIES_NAME: AnsiString = 'Порода';
  S_DICT_DAMAGE_NAME: AnsiString = 'Основная причина ослабления (усыхания)';
  S_DICT_PEST_NAME: AnsiString = 'Вид вредного организма';

  // Dictionary formats
  S_DICT_ID_FORMAT: AnsiString = '$ID$';
  S_DICT_NAME_FORMAT: AnsiString = '$NAME$';
  S_FORMAT_HELP: AnsiString = 'Подстановка значений для форматирования:' + sLineBreak +
    '$NAME$ - текстовое название' + sLineBreak + '$ID$ - идентификатор';

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

  // Script
  S_DB_TABLE_NAME: AnsiString = 'QuarterReports';
  S_DB_SCRIPT_HEADER: AnsiString = '';
  S_DB_SCRIPT_FOOTER: AnsiString = 'COMMIT;';
  S_DB_SCRIPT_LN_HEADER: AnsiString = '';
  S_DB_SCRIPT_LN_FOOTER: AnsiString = '' + sLineBreak;

  S_DB_DELETE_SCRIPT_FORMAT: AnsiString =
    'DELETE FROM %s WHERE (forestry_number = %d) ' +
    'AND (report_quarter = %d) ' +
    'AND (report_year = %d);';

  S_DB_INSERT_SCRIPT_FORMAT_BEGIN: AnsiString = 'INSERT INTO %s (';
  S_DB_INSERT_SCRIPT_FORMAT_MIDDLE: AnsiString = ') VALUES (';
  S_DB_INSERT_SCRIPT_FORMAT_END: AnsiString = ');';

  S_DB_INSERT_SCRIPT_FORMAT_FLD1: AnsiString =
    'id, input_date, report_quarter, report_year, forestry_number, local_forestry_number, kv, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD2: AnsiString =
    'patch, landuse_purpose_code, defense_category, main_species, damage_species, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD3: AnsiString =
    'main_reason, damage_year, damage_cause_group, patch_area, exam_all, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD4: AnsiString =
    'exam_fbi, exam_lesn, exam_other, dam_byear, dam_iyear, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD5: AnsiString =
    'dam_eyear, dam4, dam4_10, dam10_40, dam40, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD6: AnsiString =
    'lost_byear, lost_iyear, lost_eyear, nazn_ssr, nazn_vsr, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD7: AnsiString =
    'nazn_und, nazn_etc, nazn_etc_s, ssr_kv, ssr_kv_ar, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD8: AnsiString =
    'ssr_year, ssr_year_ar, ssr_stock, ssr_stock_ar, vsr_kv, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD9: AnsiString =
    'vsr_kv_ar, vsr_year, vsr_year_ar, vsr_stock, vsr_stock_ar, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD10: AnsiString =
    'und_kv, und_kv_ar, und_year, und_year_ar, und_stock, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD11: AnsiString =
    'und_stock_ar, etc_event_type, etc_kv, etc_kv_ar, etc_year, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD12: AnsiString =
    'etc_year_ar, etc_stock, etc_stock_ar, sost, pest, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD13: AnsiString =
    'pest_on_byear, pest_by_year, pest_likv_year, pest_zat_year, pest_eyear, ';
  S_DB_INSERT_SCRIPT_FORMAT_FLD14: AnsiString =
    'pest_eyear_mer, pest_sl, pest_sr, pest_si, pest_sp';

  S_DB_INSERT_SCRIPT_FORMAT_VAL1: AnsiString =
    '''%s'', ''%s'', %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, ';
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

  S_DB_GET_REPORT_ROW_COUNT: AnsiString =
    'select count(*) from %s where forestry_number = %d and report_quarter = %d and report_year = %d';
  S_DB_GET_SPECIES_RELATION: AnsiString =
    'select count(*) from species_causes sc join species s on s.species_id = sc.species_code join damage_causes c on c.cause_code = sc.cause_code where Upper(Trim(s.poroda)) like ''%s'' and Upper(c.cause_rus) like ''%s''';

  S_DB_GET_PREV_YEAR_SUMS: AnsiString =
    'select sum(dam_eyear), sum(lost_eyear), sum(pest_eyear) from %s where forestry_number = %d and report_year = %d - 1 and report_quarter = 4';
  S_DB_GET_PREV_QUARTER_SUMS: AnsiString =
    'select sum(dam_byear), sum(lost_byear), sum(pest_on_byear) from %s where forestry_number = %d and report_year = %d and report_quarter = %d';

  S_SQL_GET_FORESTRIES_DICT: AnsiString =
    'select distinct forestry_id, forestry_name, region_id from forestries where region_id in (22, 23) order by forestry_name';
  S_SQL_GET_LOCAL_FORESTRIES_DICT: AnsiString =
    'select distinct local_forestry_id, local_forestry_name, forestry_id from local_forestries where region_id in (22, 23) order by local_forestry_name';
  S_SQL_GET_FORESTRIES_BY_REGION: AnsiString =
    'select distinct forestry_name from forestries where region_id = %d order by forestry_name';
  S_SQL_GET_LANDUSE_DICT: AnsiString =
    'select distinct landuse_purpose_code, landuse_purpose, 0 from landuse_purposes order by landuse_purpose';
  S_SQL_GET_PROTECT_CATEGORY_DICT: AnsiString =
    'select distinct protect_category_code, protect_category_rus, 0 from protect_category order by protect_category_rus';
  S_SQL_GET_SPECIES_DICT: AnsiString =
    'select distinct species_id, poroda, 0 from species order by poroda';
  S_SQL_GET_DAMAGE_DICT: AnsiString =
    'select distinct cause_code, cause_rus, 0 from damage_causes order by cause_rus';
  S_SQL_GET_PEST_DICT: AnsiString =
    'select distinct pest_code, pest_rus, 0 from pest_dict order by pest_rus';

  ARR_FIELD_NAMES: array[1..I_COL_COUNT] of AnsiString = (
    'Лесничество',
    'Участковое лесничество',
    'Квартал',
    'Выдел',
    'Целевое назначение лесов',
    'Категория защитных лесов',
    'Преобладающая порода',
    'Повреждаемая порода',
    'Основная причина ослабления (усыхания)',
    'Год повреждения',
    'Код группы причин ослабления (усыхания)',
    'Площадь выдела',
    'Обследовано/всего нарастающим итогом (га)',
    'Обследовано/в т.ч. ФБУ "Рослесозащита" (га)',
    'Обследовано/в т.ч лесничествами (га)',
    'Обследовано/в т.ч. подрядными организациями (га)',
    'Площадь повреждения/на начало отчётного года (га)',
    'Площадь повреждения/выявлено с начала года (нарастающим итогом) (га)',
    'Площадь повреждения/на конец отчётного периода с учётом рубок (га)',
    'В том числе по степени повреждения/до 4% (га)',
    'В том числе по степени повреждения/4.1-10% (га)',
    'В том числе по степени повреждения/10.1-40% (га)',
    'В том числе по степени повреждения/более 40% (га)',
    'В том числе погибшие насаждения/на начало отчётного года (га)',
    'В том числе погибшие насаждения/нарастающим итогом (га)',
    'В том числе погибшие насаждения/с начала года	оставшиеся на корню на конец отчётного периода (с учётои рубок) (га)',
    'Назначено мероприятий/ССР (га)',
    'Назначено мероприятий/ВСР (га)',
    'Назначено мероприятий/УЗ (га)',
    'Назначено мероприятий/Прочие/Вид мероприятия',
    'Назначено мероприятий/Прочие/Площадь (га)',
    'Проведено мероприятий/ССР/За отчётный период/Всего (га)',
    'Проведено мероприятий/ССР/За отчётный период/В т.ч. на арендованных территориях (га)',
    'Проведено мероприятий/ССР/Нарастающим итогом с начала года/Всего (га)',
    'Проведено мероприятий/ССР/Нарастающим итогом с начала года/В т.ч. на арендованных территориях (га)',
    'Проведено мероприятий/ССР/Выбираемый запас/Всего (м3)',
    'Проведено мероприятий/ССР/Выбираемый запас/В т.ч. на арендованных территориях (м3)',
    'Проведено мероприятий/ВСР/За отчётный период/Всего (га)',
    'Проведено мероприятий/ВСР/За отчётный период/В т.ч. на арендованных территориях (га)',
    'Проведено мероприятий/ВСР/Нарастающим итогом с начала года/Всего (га)',
    'Проведено мероприятий/ВСР/Нарастающим итогом с начала года/В т.ч. на арендованных территориях (га)',
    'Проведено мероприятий/ВСР/Выбираемый запас/Всего (м3)',
    'Проведено мероприятий/ВСР/Выбираемый запас/В т.ч. на арендованных территориях (м3)',
    'Проведено мероприятий/УЗ/За отчётный период/Всего (га)',
    'Проведено мероприятий/УЗ/За отчётный период/В т.ч. на арендованных территориях (га)',
    'Проведено мероприятий/УЗ/Нарастающим итогом с начала года/Всего (га)',
    'Проведено мероприятий/УЗ/Нарастающим итогом с начала года/В т.ч. на арендованных территориях (га)',
    'Проведено мероприятий/УЗ/Выбираемый запас/Всего (м3)',
    'Проведено мероприятий/УЗ/Выбираемый запас/В т.ч. на арендованных территориях (м3)',
    'Проведено мероприятий/Прочие мероприятия/Вид мероприятия',
    'Проведено мероприятий/Прочие мероприятия/За отчётный период/Всего (га)',
    'Проведено мероприятий/Прочие мероприятия/За отчётный период/В т.ч. на арендованных территориях (га)',
    'Проведено мероприятий/Прочие мероприятия/Нарастающим итогом с начала года/Всего (га)',
    'Проведено мероприятий/Прочие мероприятия/Нарастающим итогом с начала года/В т.ч. на арендованных территориях (га)',
    'Проведено мероприятий/Прочие мероприятия/Выбираемый запас/Всего (м3)',
    'Проведено мероприятий/Прочие мероприятия/Выбираемый запас/В т.ч. на арендованных территориях (м3)',
    'Информация об очагах вредителей и болезней/Состояние очага',
    'Информация об очагах вредителей и болезней/Вид вредного организма',
    'Информация об очагах вредителей и болезней/Площадь очага/На начало отчетного года (га)',
    'Информация об очагах вредителей и болезней/Площадь очага/Возникло вновь (га)',
    'Информация об очагах вредителей и болезней/Площадь очага/Ликвидировано (га)',
    'Информация об очагах вредителей и болезней/Площадь очага/Затухло под воздействием естественных факторов (га)',
    'Информация об очагах вредителей и болезней/Площадь очага/На конец отчетного квартала/Всего (га)',
    'Информация об очагах вредителей и болезней/Площадь очага/На конец отчетного квартала/в т.ч. требует мер борьбы (га)',
    'Степень очага/Слабая (га)',
    'Степень очага/Средняя (га)',
    'Степень очага/Сильная (га)',
    'Степень очага/Сплошная (га)'
    );

  // =)
  S_YES = 'True';
  S_NO = 'False';
  S_FREE_SELECT: AnsiString = 'SELECT';
  S_DOTCOMMA_SELECT: AnsiString = ';SELECT';
  S_BRACKED1_SELECT: AnsiString = '(SELECT';
  S_BRACKED2_SELECT: AnsiString = ')SELECT';
  S_EQUAL_SELECT: AnsiString = '=SELECT';
  S_COMMA_SELECT: AnsiString = ',SELECT';
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

