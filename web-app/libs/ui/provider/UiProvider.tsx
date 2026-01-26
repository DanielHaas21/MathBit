'use client';
import React, { createContext, useContext } from 'react';
import { ToastProvider } from '../components/Toast/ToastProvider';
import type { i18n as I18nInstance, TOptionsBase } from 'i18next';

// I18n context
const I18nContext = createContext<I18nInstance | null>(null);

export const useTranslation = (root: string) => {
  const i18n = useContext(I18nContext);
  return (key: string, options?: unknown) =>
    i18n
      ? (i18n.t(
          `${root}.${key}`,
          options as TOptionsBase & Record<string, unknown>
        ) as React.ReactNode | string)
      : `${root}.${key}`;
};

export const useI18n = () => {
  const i18n = useContext(I18nContext);
  if (!i18n) throw new Error('I18n not initialized');
  return i18n;
};

interface UiProviderProps {
  children: React.ReactNode;
  i18nValue: I18nInstance; // value for I18nContext
}

export function UiProvider({ children, i18nValue }: UiProviderProps) {
  return (
    <I18nContext.Provider value={i18nValue}>
      <ToastProvider>{children}</ToastProvider>
    </I18nContext.Provider>
  );
}
