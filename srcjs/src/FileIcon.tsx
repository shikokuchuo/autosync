// Small file-type icon shown beside the open file's name in the editor header.
// Quarto (.qmd) gets the official Quarto glyph; everything else gets a neutral
// document icon so the header stays visually consistent.

interface FileIconProps {
  path: string;
  className?: string;
}

// Official Quarto mark (Simple Icons, viewBox 0 0 24 24), brand colour #39729E.
export const QUARTO_PATH =
  "M12.65 12.854V24c6.042-.325 10.923-5.105 11.33-11.125H12.65Zm-1.504 0H.02c.427 " +
  "5.94 5.166 10.699 11.105 11.105V12.854Zm1.505-1.505H24C23.675 5.247 18.753.325 " +
  "12.65 0Zm-1.505 0V0C5.106.427.326 5.308 0 11.35Z";

function extOf(path: string): string {
  const m = path.toLowerCase().match(/\.([^./\\]+)$/);
  return m ? m[1] : "";
}

export function FileIcon({ path, className }: FileIconProps) {
  if (extOf(path) === "qmd") {
    return (
      <svg
        className={className}
        width="14"
        height="14"
        viewBox="0 0 24 24"
        fill="#39729e"
        aria-hidden="true"
      >
        <path d={QUARTO_PATH} />
      </svg>
    );
  }
  return (
    <svg
      className={className}
      width="14"
      height="14"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.3"
      strokeLinejoin="round"
      aria-hidden="true"
    >
      <path d="M4 1.6h5l3.5 3.6V14a.4.4 0 0 1-.4.4H4a.4.4 0 0 1-.4-.4V2a.4.4 0 0 1 .4-.4Z" />
      <path d="M9 1.6v3.6h3.5" />
    </svg>
  );
}
