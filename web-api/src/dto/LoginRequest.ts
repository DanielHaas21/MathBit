/**
 * Represents a login request using username and password.
 */
export interface LoginRequest {
  /**
   * The login username.
   */
  username: string;

  /**
   * The login password.
   */
  password: string;
}
