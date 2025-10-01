import React, { useId } from 'react';

type RadioBoolProps = {
  value: boolean | undefined | null;
  onChange: (v: boolean) => void;
  falseText: string; // left label
  trueText: string; // right label
  name?: string; // optional name override
  error?: string | null;
  disabled?: boolean;
  className?: string;
};

export function RadioBool({
  value,
  onChange,
  falseText,
  trueText,
  name,
  error,
  disabled,
  className
}: RadioBoolProps) {
  const autoName = useId();
  const groupName = name ?? autoName;

  return (
    <div
      className={`flex items-center gap-6 ${className ?? ''}`}
      role="radiogroup"
      aria-invalid={!!error}
    >
      {/* true option */}
      <label className="inline-flex items-center gap-2 cursor-pointer">
        <input
          type="radio"
          name={groupName}
          value="true"
          checked={value === true}
          onChange={() => onChange(true)}
          disabled={disabled}
          className="h-4 w-4"
        />
        <span>{trueText}</span>
      </label>

      {/* false option */}
      <label className="inline-flex items-center gap-2 cursor-pointer">
        <input
          type="radio"
          name={groupName}
          value="false"
          checked={value === false}
          onChange={() => onChange(false)}
          disabled={disabled}
          className="h-4 w-4"
        />
        <span>{falseText}</span>
      </label>

      {error && <p className="text-red-600 text-sm ml-2">{error}</p>}
    </div>
  );
}
