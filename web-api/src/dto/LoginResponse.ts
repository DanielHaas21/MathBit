import { UserProfile } from '@/types/userProfile';

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
  /**
   * The login user payload
   */
  userProfile?: {
    id?: number | null;
    username?: string | null | undefined;
    email?: string | null | undefined;
  };
}
