/**
 * Represents a login request using email and password.
 * Password is compared via bcrypt, both email, and password is correspondent to the to the interface @interface User
 */
export interface LoginRequest {
  /**
   * The login username.
   */
  email: string;

  /**
   * The login password.
   */
  password: string;
}
