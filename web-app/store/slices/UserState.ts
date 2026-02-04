import { createSlice, PayloadAction } from '@reduxjs/toolkit';
// Same as the one in web-api
export interface UserProfile {
  username?: string | null | undefined;
  email?: string | null | undefined;
  id?: number | null;
}

export interface UserState {
  accessToken: string | undefined;
  user: UserProfile | null;
  authStatus: 'unknown' | 'authenticated' | 'unauthenticated';
}

const initialState: UserState = {
  user: {
    id: null,
    email: undefined,
    username: undefined,
  },
  accessToken: undefined,
  authStatus: 'unknown',
};

const UserSlice = createSlice({
  name: 'User',
  initialState,
  reducers: {
    login: (state, action: PayloadAction<UserState>) => {
      state.user = action.payload.user;
      state.accessToken = action.payload.accessToken;
      state.authStatus = 'authenticated';
    },
    logout: (state) => {
      state.user = null;
      state.accessToken = undefined;
      state.authStatus = 'unauthenticated';
    },
  },
});

export const { login, logout } = UserSlice.actions;

export default UserSlice.reducer;
