/**
 * Represents a login response
 */
export interface LoginResponse {
  /**
   * The access token.
   */
  token?: string;
  /**
   * The access token.
   */
  refreshToken?: string;
  /**
   * The login error code.
   */
  errorCode?: string;
}
