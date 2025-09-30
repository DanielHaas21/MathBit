/**
 * Represents the data required to create a new user
 */
export interface CreateUser {
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
   * User's email address
   */
  email: string;

  /**
   * User's password
   */
  password: string;
}
