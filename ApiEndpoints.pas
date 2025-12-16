unit ApiEndpoints;

interface

const
  // Base URL
  API_BASE_URL = 'https://api.example.com';
  
  // Auth endpoints
  ENDPOINT_LOGIN = '/auth/login';
  ENDPOINT_REFRESH_TOKEN = '/auth/refresh';
  ENDPOINT_LOGOUT = '/auth/logout';
  
  // User endpoints
  ENDPOINT_USERS_LIST = '/api/users';
  ENDPOINT_USER_ME = '/api/users/me';
  ENDPOINT_USER_BY_ID = '/api/users/{id}';
  
  // Token storage file names
  TOKEN_ACCESS = 'access_token';
  TOKEN_REFRESH = 'refresh_token';
  TOKEN_EXPIRY = 'token_expiry';
  
  // Config
  CONFIG_FILE = 'config.ini';
  CONFIG_SECTION = 'Security';
  CONFIG_SAFE_TOKENS = 'SAFE_TOKENS';

implementation

end.
