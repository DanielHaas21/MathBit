'use client';
import React, { createContext, useContext } from 'react';
import { ToastProvider } from '../components/Toast/ToastProvider';

// I18n context
const I18nContext = createContext<{ t: (key: string) => string }>({ t: () => '' });

export const useTranslation = () => {
  const { t } = useContext(I18nContext);
  return t;
};

interface UiProviderProps {
  children: React.ReactNode;
  i18nValue: { t: (key: string) => string }; // value for I18nContext
}

export function UiProvider({ children, i18nValue }: UiProviderProps) {
  return (
    <I18nContext.Provider value={i18nValue}>
      <ToastProvider>{children}</ToastProvider>
    </I18nContext.Provider>
  );
}
