import { Request } from 'express';
import { UserProfile } from './userProfile';

export interface ExpressRequest extends Request {
  userProfile?: UserProfile | null | undefined;
  token?: string;
}
