import axios, { AxiosError, AxiosRequestConfig } from 'axios';
import { refresh } from 'web-api-client';
import { store } from '@/store/store';
import { login as loginState } from '@/store/slices/UserState';

// interceptor to handle 401 responses and attempt token refresh
