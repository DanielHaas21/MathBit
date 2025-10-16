import * as jwt from 'jsonwebtoken';
import { getAppConfig } from './appConfig';
import { ExpressRequest } from './types/expressRequest';

const config = getAppConfig();

export function expressAuthentication(request: ExpressRequest, securityName: string): Promise<any> {
  if (securityName === 'jwt') {
    return new Promise<any>((resolve, reject) => {
      const [authType, token] = request.headers['authorization']?.split(' ') ?? [];

      if (authType !== 'Bearer') {
        return reject(new Error('Unknown auth type'));
      }
      if (!token) {
        return reject(new Error('No token provided'));
      }

      jwt.verify(token, config.app.secret, (err, decoded) => {
        if (err) {
          return reject(err);
        }
        if (decoded) {
          const data = decoded as { id: number; username: string; email: string };
          request.userProfile = { id: data.id, username: data.username, email: data.email };
          request.token = token;
          return resolve(decoded);
        }
      });
    });
  }

  return Promise.reject(new Error('Unknown auth type'));
}
