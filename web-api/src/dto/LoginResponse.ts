/**
 * Represents a login response
 */
export interface LoginResponse {
  /**
   * The access token.
   */
  refreshToken?: string;
  /**
   * The access token.
   */
  accessToken?: string;
  /**
   * The login error code.
   */
  errorCode?: string;
}
