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
}

const initialState: UserState = {
  user: {
    id: null,
    email: undefined,
    username: undefined,
  },
  accessToken: undefined,
};

const UserSlice = createSlice({
  name: 'User',
  initialState,
  reducers: {
    login: (state, action: PayloadAction<UserState>) => {
      state.user = action.payload.user;
      state.accessToken = action.payload.accessToken;
    },
    logout: (state) => {
      state.user = null;
      state.accessToken = undefined;
    },
  },
});

export const { login, logout } = UserSlice.actions;

export default UserSlice.reducer;
