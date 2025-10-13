/**
 * Represents the data required to update user
 */
export interface UpadteUser {
  /**
   * Unique identifier for the user
   */
  id?: number;
  /**
   * User's displayed username
   */
  username: string;
  /**
   * User's first name
   */
  firstName: string;

  /**
   * User's last name
   */
  lastName: string;

  /**
   * User's password
   */
  password: string;

  /**
   * User's email address
   */
  email: string;
}
