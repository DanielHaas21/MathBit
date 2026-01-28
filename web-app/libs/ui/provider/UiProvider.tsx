'use client';
import React, { createContext, useContext } from 'react';
import { ToastProvider } from '../components/Toast/ToastProvider';
import type { i18n as I18nInstance, TOptionsBase } from 'i18next';

export type TranslationValue = string | number | React.ReactNode;
type TranslationOptions = TOptionsBase & Record<string, unknown>;
// I18n context
const I18nContext = createContext<I18nInstance | null>(null);

export const useTranslation = (root: string) => {
  const i18n = useContext(I18nContext);

  function t(key: string): string;
  function t(key: string, options: TranslationOptions): TranslationValue;
  function t(key: string, options?: TranslationOptions): TranslationValue {
    if (!i18n) return `${root}.${key}`;

    return i18n.t(`${root}.${key}`, options);
  }

  return t;
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
