import { store } from '@/store/store';
import { UpadteUser, updateUser, refresh } from 'web-api-client';
import { login as loginState } from '@/store/slices/UserState';
import getApiConfig from '@/apiConfig';
import { ActionResult } from '../types/ActionResult';

async function update(id: number, args: UpadteUser): Promise<ActionResult> {
  try {
    const data = args.password
      ? {
          email: args.email,
          username: args.username,
          password: args.password,
        }
      : { email: args.email, username: args.username };

    const updateAction = await updateUser({ id: id, user: data }, getApiConfig(false));

    store.dispatch(
      loginState({
        ...store.getState().User,
        user: {
          ...data,
        },
      })
    );

    await refresh(getApiConfig(true));

    return { ok: true };
  } catch (error) {
    throw new Error('Login failed', error as ErrorOptions);
  }
}

export default update;
